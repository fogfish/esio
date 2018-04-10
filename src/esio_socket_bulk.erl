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
-include_lib("datum/include/datum.hrl").

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
   [200 | State] = http( elastic_ping(Uri, Opts) ),   
   {ok, handle, 
      State#{
         uri   => Uri,
         n     => opts:val(n, ?CONFIG_BULK_CHUNK, Opts),
         t     => tempus:timer(opts:val(t, ?CONFIG_T_SYNC, Opts), sync),
         chunk => q:new()
      }
   }.

free(_, State) ->
   [identity || elastic_sync(State), http(_)],
   ok.

%%-----------------------------------------------------------------------------
%%
%% state machine
%%
%%-----------------------------------------------------------------------------

handle({put, _, _} = Req, Pipe, State) ->
   {next_state, handle, 
      [identity ||
         enq(Req, State),
         elastic_bulk(_),
         http(_),
         ack(Pipe, _)
      ]
   };

handle(sync, _, State) ->
   {next_state, handle,
      [identity ||
         elastic_sync(State),
         http(_),
         timeout(_)
      ]
   };

handle({http, _, passive}, Pipe, State) ->
   pipe:a(Pipe, {active, 1024}),
   {next_state, handle, State}.


%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
enq(Req, #{chunk := Chunk} = State) ->
   State#{chunk => q:enq(Req, Chunk)}.

%%
http([?None | State0]) ->
   [ok | State0];

http([Http | State0]) ->
   case Http(State0) of
      [ok | State1] ->
         [ok | maps:without([req, ret], State1#{chunk => q:new()})];
      [Result | State1] ->
         [Result | maps:without([req, ret], State1)]
   end;

http(Http) ->
   http([Http | #{}]).

%%
ack(Pipe, [Result | State]) ->
   pipe:a(Pipe, Result),
   State.

%%
timeout([_ | #{t := T} = State]) ->
   State#{t => tempus:reset(T, sync)}.

%%
elastic_ping(Uri, Opts) ->
   [m_http ||
      cats:new(uri:path(<<"/_cat/nodes">>, Uri)),
      cats:so(Opts),
      cats:method('GET'),
      cats:header('Connection', 'keep-alive'),
      cats:request(60000),
      cats:require(code, 200)
   ].

%%
elastic_bulk(#{chunk := Chunk, n := N} = State) ->
   case q:length(Chunk) of
      X when X >= N ->
         [elastic_bulk_req(State) | State];
      _ ->
         [?None | State]
   end.

elastic_bulk_req(#{chunk := Chunk, uri := Uri}) ->
   [m_http ||
      cats:new( uri:path(<<"/_bulk">>, Uri) ),
      cats:method('POST'),
      cats:header('Content-Type', "application/x-ndjson"),
      cats:header('Transfer-Encoding', "chunked"),
      cats:header('Connection', 'keep-alive'),
      cats:payload( encode(bucket(Uri), Chunk) ),
      cats:request(60000),
      cats:unit( elastic_bulk_ret(_) )
   ].

elastic_bulk_ret([{200, _, _}, Json]) ->
   %% @todo: bulk api response is not very efficient to ack the request.
   %%        the library implements fire-and-forget  
   %%        however, it is possible to analyses results of bulk response and
   %%        filter out successful objects
   ok;
elastic_bulk_ret([{Code, _, _}, Reason]) ->
   {error, {Code, Reason}}.


%%
elastic_sync(#{chunk := Chunk} = State) ->
   case q:length(Chunk) of
      X when X > 0 ->
         [elastic_bulk_req(State) | State];
      _ ->
         [?None | State]
   end.


%%
bucket(Uri) ->
   hd(uri:segments(Uri)).

%%
%%
encode(Bucket, Chunk) ->
   [identity ||
      q:map(fun({put, Key, Val}) -> encode(Bucket, Key, Val) end, Chunk),
      q:list(_),
      erlang:iolist_to_binary(_)
   ].

encode(Bucket, Key, Val) ->
   Head = #{index => 
      #{
         <<"_index">> => Bucket, 
         <<"_type">>  => <<"_doc">>, 
         <<"_id">>    => Key
      }
   },
   <<(jsx:encode(Head))/binary, $\n, (jsx:encode(Val))/binary, $\n>>.
