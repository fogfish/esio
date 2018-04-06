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
               [either ||
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
               [either ||
                  identity_bulk(Uri),
                  http_bulk(_, Chunk0, Opts),
                  http_run(_, State0)
               ]
            )
         }
   end;

handle(close, _Pipe, #{uri := Uri, opts := Opts, chunk := Chunk0, t := T} = State0) ->
   %% We should not discard existing messages so let's send them immediately, and then stop.
   tempus:cancel(T),
   case deq:length(Chunk0) of
      0 -> ok;
      _ ->
         http_return(State0,
                     [either ||
                        identity_bulk(Uri),
                        http_bulk(_, Chunk0, Opts),
                        http_run(_, State0)
                     ]
                  )
   end,
   {stop, normal, State0#{chunk => q:new()}}.


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
      [m_http ||
         cats:new(Uri, Opts),
         cats:x('PUT'),
         cats:h('Content-Type', "application/json"),
         cats:h('Transfer-Encoding', "chunked"),
         cats:h('Connection', 'keep-alive'),
         cats:d(encode(uri:new(Uri), Chunk)),
         cats:request(60000),
         cats:unit(http_bulk_return(_))
      ]
   }.

http_bulk_return([{200, _, _}|_]) ->
   %% @todo: bulk api response is not very efficient to ack the request.
   %%        the library implements fire-and-forget  
   ok;
http_bulk_return([{Code, _, _}|_]) ->
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
   case deq:head(Chunk) of
      undefined -> [];
      {_, Key, Val} -> 
         [encode(Uri, Key, Val) | encode(Uri, deq:tail(Chunk))]
   end.

encode(Uri, {urn, Type, Key}, Val) ->
   Cask = hd(uri:segments(Uri)),
   Head = jsx:encode(#{index => 
      #{<<"_index">> => uri:unescape(Cask), <<"_type">> => uri:unescape(Type), <<"_id">> => uri:unescape(Key)}
   }),
   <<Head/binary, $\n, Val/binary, $\n>>.

