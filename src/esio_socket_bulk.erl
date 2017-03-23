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
-compile({parse_transform, category}).
-compile({parse_transform, monad}).
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
   {ok, handle, 
      #{
         uri   => Uri, 
         opts  => Opts,
         t     => tempus:timer(opts:val(t, ?CONFIG_T_SYNC, Opts), sync),
         n     => opts:val(n, ?CONFIG_BULK_CHUNK, Opts),
         chunk => q:new()
      }
   }.

free(_, _) ->
   ok.

%%-----------------------------------------------------------------------------
%%
%% state machine
%%
%%-----------------------------------------------------------------------------

handle({put, _, _} = Req, Pipe, #{uri := Uri, opts := Opts, chunk := Chunk0, n := N} = State0) ->
   case 
      {q:enq(Req, Chunk0), deq:length(Chunk0) + 1}
   of
      {Chunk1, Len} when Len >= N ->
         {next_state, handle,
            http_return(Pipe, State0,
               [$^ ||
                  identity_bulk(Uri),
                  http_bulk(_, Chunk1, Opts),
                  http_run(_, State0)
               ]
            )
         };
      
      {Chunk1, _} ->
         pipe:a(Pipe, ok),
         {next_state, handle, State0#{chunk => Chunk1}}
   end;

handle(sync, _, #{uri := Uri, opts := Opts, chunk := Chunk0, t := T} = State0) ->
   case deq:length(Chunk0) of
      0 -> 
         {next_state, handle,
            State0#{t => tempus:reset(T, sync)}
         };
      _ ->
         {next_state, handle,
            http_return(State0,
               [$^ ||
                  identity_bulk(Uri),
                  http_bulk(_, Chunk0, Opts),
                  http_run(_, State0)
               ]
            )
         }
   end.


%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%%
http_run(Http, State) ->
   {ok, Http(State)}.

%%
%%
http_return(Pipe, _, {ok, [Result | State]}) -> 
   pipe:a(Pipe, Result),
   State#{chunk => q:new()};

http_return(Pipe, State, {error, _} = Error) ->
   pipe:a(Pipe, Error),
   State.

%%
%%
http_return(_, {ok, [_ | State]}) -> 
   State#{chunk => q:new()};

http_return(State, {error, _}) ->
   State.

%%
%%
http_bulk(Uri, Chunk, Opts) ->
   {ok, 
      do([m_http ||
         _ /= new(Uri, Opts),
         _ /= x('PUT'),
         _ /= h('Content-Type', "application/json"),
         _ /= h('Transfer-Encoding', "chunked"),
         _ /= h('Connection', 'keep-alive'),
         _ /= d(encode(uri:new(Uri), Chunk)),
         _ /= request(60000),
         _ =< http_bulk_return(_),
         return(_)
      ])
   }.

http_bulk_return([{200, _, _, _}|_]) ->
   %% @todo: bulk api response is not very efficient to ack the request.
   %%        the library implements fire-and-forget  
   ok;
http_bulk_return([{Code, _, _, _}|_]) ->
   {error, Code}.


%%
%%
identity_bulk(Uri) ->
   case uri:segments(Uri) of
      [Cask | _] -> 
         {ok, uri:s(uri:segments([Cask, <<"_bulk">>], Uri))};
      Path -> 
         {error, {badkey, Path}}
   end.

%%
%%
encode(_Uri, {}) ->
   [];
encode(Uri, Chunk) ->
   {_, Key, Val} = deq:head(Chunk),
   [encode(Uri, Key, Val) | encode(Uri, deq:tail(Chunk))].

encode(Uri, {urn, Type, Key}, Val) ->
   Cask = hd(uri:segments(Uri)),
   Head = jsx:encode(#{index => 
      #{<<"_index">> => uri:unescape(Cask), <<"_type">> => uri:unescape(Type), <<"_id">> => uri:unescape(Key)}
   }),
   <<Head/binary, $\n, Val/binary, $\n>>.

