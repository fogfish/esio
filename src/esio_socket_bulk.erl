%%
%%   Copyright (C) 2016 Zalando SE
%%
%%   This software may be modified and distributed under the terms
%%   of the MIT license.  See the LICENSE file for details.
%%
%% @doc
%%   Elastic Search REST API bulk socket
-module(esio_socket_bulk).
-behaviour(pipe).
-author('dmitry.kolesnikov@zalando.fi').

-include("esio.hrl").

-export([
   start_link/2,
   init/1,
   free/2,
   handle/3
]).


%%-----------------------------------------------------------------------------
%%
%% factory
%%
%%-----------------------------------------------------------------------------

start_link(Uri, Opts) ->
   pipe:start_link(?MODULE, [Uri, Opts], []).   

init([Uri, Opts]) ->
   erlang:process_flag(trap_exit, true),
   Sock = knet:socket(Uri, Opts),
   {ok, handle, 
      #{
         sock  => Sock, 
         uri   => Uri, 
         opts  => Opts,
         t     => tempus:timer(opts:val(t, ?CONFIG_T_SYNC, Opts), sync),
         n     => opts:val(n, ?CONFIG_BULK_CHUNK, Opts),
         req   => deq:new(),
         chunk => deq:new()
      }
   }.

free(_, #{sock := Sock}) ->
   knet:close(Sock).

%%-----------------------------------------------------------------------------
%%
%% state machine
%%
%%-----------------------------------------------------------------------------

%%
%% client requests
handle({put, _, _}=Put, Pipe, #{sock := Sock, uri := Uri, req := Req, chunk := Chunk0, n := N} = State) ->
   case 
      {deq:enq(Put, Chunk0), deq:length(Chunk0) + 1}
   of
      {Chunk1, Len} when Len >= N ->
         request(Sock, build_http_req(Uri, Chunk1)),
         {next_state, handle, 
            State#{req => deq:enq(#{pipe => Pipe}, Req), chunk => deq:new()}
         };

      {Chunk1,   _} ->
         pipe:a(Pipe, ok),
         {next_state, handle, 
            State#{chunk => Chunk1}
         }
   end;

handle(sync, _, #{sock := Sock, uri := Uri, req := Req, chunk := Chunk0, t := T} = State) ->
   case deq:length(Chunk0) of
      0 -> 
         {next_state, handle,
            State#{t => tempus:reset(T, sync)}
         };
      _ ->
         request(Sock, build_http_req(Uri, Chunk0)),
         {next_state, handle, 
            State#{req => deq:enq(#{}, Req), chunk => deq:new(), t => tempus:reset(T, sync)}
         }
   end;

%%
%% socket is terminated 
handle({sidedown, b, normal}, _, #{uri := Uri, opts := Opts, req := Req} = State) ->
   lists:foreach(
      fun(#{pipe := Pipe}) -> pipe:a(Pipe, {error, 503}) end,
      deq:list(Req)
   ),
   Sock = knet:socket(Uri, Opts),
   {next_state, handle, 
      State#{sock => Sock}
   };

handle({sidedown, b, Reason}, _, State) ->
   {stop, Reason, State};

handle(close, _, State) ->
   {stop, normal, State};

%%
%% elastic search response
handle({http, _Sock, {Code, _Text, _Head, _Env}}, _Pipe, #{req := Req} = State) ->
   Head = deq:head(Req),
   {next_state, handle, 
      State#{req => deq:poke(Head#{code => Code, json => []}, deq:tail(Req))}
   };

handle({http, _Sock,  eof}, _Pipe, #{req := Req} = State) ->
   response(deq:head(Req)),
   {next_state, handle, 
      State#{req => deq:tail(Req)}
   };

handle({http, _Sock, Pack}, _Pipe, #{req := Req} = State) ->
   #{json := Json} = Head = deq:head(Req),
   {next_state, handle, 
      State#{req => deq:poke(Head#{json => [Pack|Json]}, deq:tail(Req))}
   }.


%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%% stream request to elastic search
request(Sock, Packets) ->
   lists:foreach(
      fun(X) ->
         knet:send(Sock, X)
      end,
      Packets
   ).

%%
%% stream response to client
response(#{code := _Code, pipe := Pipe})  ->
   %% @todo: bulk api response is not very efficient to ack the request.
   %%        the library implements fire-and-forget  
   pipe:a(Pipe, ok);

response(_) ->
   %% sync successful, no ack is required
   ok.


%%
%% 
build_http_req(Uri, Chunk) ->
   [
      {
         'PUT',
         uri:path(<<"/_bulk">>, Uri),
         [
            {'Content-Type',  {application, json}},
            {'Transfer-Encoding', <<"chunked">>},
            {'Connection',     'keep-alive'}
         ]
      } 
     |encode(Uri, Chunk)
   ].


%%
%%
encode(_Uri, {}) ->
   [eof];
encode(Uri, Chunk) ->
   {_, Key, Val} = deq:head(Chunk),
   [encode(Uri, Key, Val) | encode(Uri, deq:tail(Chunk))].

encode(Uri, Key, Val) ->
   [Cask, Type, Id] = uri:segments(esio_identity:keyval(Uri, Key)),
   Head = jsx:encode(#{index => 
      #{<<"_index">> => uri:unescape(Cask), <<"_type">> => uri:unescape(Type), <<"_id">> => uri:unescape(Id)}
   }),
   <<Head/binary, $\n, Val/binary, $\n>>.

