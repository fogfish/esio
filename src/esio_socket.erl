%%
%%   Copyright (C) 2016 Zalando SE
%%
%%   This software may be modified and distributed under the terms
%%   of the MIT license.  See the LICENSE file for details.
%%
%% @doc
%%   Elastic Search REST API socket
-module(esio_socket).
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
   [200 | State] = http( elastic_ping(Uri, maps:from_list(Opts)) ),
   {ok, handle, State#{uri => Uri}}.

free(_, _) ->
   ok.

%%-----------------------------------------------------------------------------
%%
%% state machine
%%
%%-----------------------------------------------------------------------------
handle(schema, Pipe, #{uri := Uri} = State) ->
   {next_state, handle,
      [identity ||
         elastic_get(Uri),
         http(_, State),
         ack(Pipe, _)
      ]
   };

handle({schema, #{properties := _} = Json}, Pipe, #{uri := Uri} = State) ->
   {next_state, handle,
      [identity ||
         identity_schema(Uri),
         elastic_put(_, Json),
         http(_, State),
         ack(Pipe, _)
      ]
   };

handle({schema, Json}, Pipe, #{uri := Uri} = State) ->
   {next_state, handle,
      [identity ||
         elastic_put(Uri, Json),
         http(_, State),
         ack(Pipe, _)
      ]
   };

handle({put, Key, Json}, Pipe, #{uri := Uri} = State) ->
   {next_state, handle,
      [identity ||
         identity_key(Key, Uri),
         elastic_put(_, Json),
         http(_, State),
         ack(Pipe, _)
      ]
   };

handle({add, Json}, Pipe, #{uri := Uri} = State) ->
   {next_state, handle,
      [identity ||
         identity_key(Uri),
         elastic_add(_, Json),
         http(_, State),
         ack(Pipe, _)
      ]
   };

handle({get, Key}, Pipe, #{uri := Uri} = State) ->
   {next_state, handle,
      [identity ||
         identity_key(Key, Uri),
         elastic_get(_),
         http(_, State),
         ack(Pipe, _)
      ]
   };

handle({remove, Key}, Pipe, #{uri := Uri} = State)
 when is_binary(Key) ->
   {next_state, handle,
      [identity ||
         identity_key(Key, Uri),
         elastic_remove(_),
         http(_, State),
         ack(Pipe, _)
      ]
   };

handle({remove, Query}, Pipe, #{uri := Uri} = State)
 when is_map(Query) ->
   {next_state, handle,
      [identity ||
         identity_q(Uri, <<"_delete_by_query">>),
         elastic_lookup(_, Query),
         http(_, State),
         ack(Pipe, _)
      ]
   };


handle({update, Key, Json}, Pipe, #{uri := Uri} = State) ->
   {next_state, handle,
      [identity ||
         identity_update(Key, Uri),
         elastic_update(_, Json),
         http(_, State),
         ack(Pipe, _)
      ]
   };


handle({lookup, Query}, Pipe, #{uri := Uri} = State) ->
   {next_state, handle,
      [identity ||
         identity_q(Uri, <<"_search">>),
         elastic_lookup(_, Query),
         http(_, State),
         ack(Pipe, _)
      ]
   };

handle({lookup, Bucket, Query}, Pipe, #{uri := Uri} = State) ->
   {next_state, handle,
      [identity ||
         uri:segments([Bucket, <<"_search">>], Uri),
         elastic_lookup(_, Query),
         http(_, State),
         ack(Pipe, _)
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
http(Http) ->
   http(Http, #{}).

http(Http, State0) ->
   [Result | State1] = Http(State0),
   [Result | maps:without([req, ret], State1)].


%%
ack(Pipe, [Result | State]) ->
   pipe:a(Pipe, Result),
   State.


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
elastic_put(Uri, Json) ->
   [m_http ||
      cats:new(Uri),
      cats:method('PUT'),
      cats:header('Content-Type', "application/json"),
      cats:header('Transfer-Encoding', "chunked"),
      cats:header('Connection', "keep-alive"),
      cats:payload(Json),
      cats:request(60000),
      cats:unit( elastic_put_return(_) )
   ].

elastic_put_return([{Code, _, _}, #{<<"_id">> := Key}])
 when Code =:= 201 orelse Code =:= 200 ->
   {ok, Key};

elastic_put_return([{Code, _, _}, #{<<"acknowledged">> := true, <<"index">> := Key}])
 when Code =:= 201 orelse Code =:= 200 ->
   {ok, Key};

elastic_put_return([{Code, _, _}, #{<<"acknowledged">> := true}])
 when Code =:= 201 orelse Code =:= 200 ->
   {ok, acknowledged};

elastic_put_return([{Code, _, _}, Reason]) ->
   {error, {Code, Reason}}.


%%
elastic_add(Uri, Json) ->
   [m_http ||
      cats:new(Uri),
      cats:method('POST'),
      cats:header('Content-Type', "application/json"),
      cats:header('Transfer-Encoding', "chunked"),
      cats:header('Connection', 'keep-alive'),
      cats:payload(Json),
      cats:request(60000),
      cats:unit( elastic_put_return(_) )
   ].


%%
elastic_get(Uri) ->
   [m_http ||
      cats:new(Uri),
      cats:method('GET'),
      cats:header('Accept', "application/json"),
      cats:header('Connection', 'keep-alive'),
      cats:request(60000),
      cats:unit( elastic_get_return(_) )
   ].

elastic_get_return([{200, _, _}, #{<<"_source">> := Json}]) ->
   {ok, Json};

elastic_get_return([{200, _, _}, Json]) ->
   {ok, Json};

elastic_get_return([{404, _, _} | _]) ->
   {error, not_found};

elastic_get_return([{Code, _, _}, Reason]) ->
   {error, {Code, Reason}}.


%%
elastic_remove(Uri) ->
   [m_http ||
      cats:new(Uri),
      cats:method('DELETE'),
      cats:header('Accept', "application/json"),
      cats:header('Connection', 'keep-alive'),
      cats:request(60000),
      cats:unit( elastic_remove_return(_) )
   ].

elastic_remove_return([{Code, _, _}, #{<<"_id">> := Key}])
 when Code >= 200, Code < 300 orelse Code =:= 404 ->
   {ok, Key};
elastic_remove_return([{Code, _, _}, Reason]) ->
   {error, {Code, Reason}}.

%%
elastic_update(Uri, Json) ->
   [m_http ||
      cats:new(Uri),
      cats:method('POST'),
      cats:header('Content-Type', "application/json"),
      cats:header('Transfer-Encoding', "chunked"),
      cats:header('Connection', "keep-alive"),
      cats:payload(#{doc => Json, doc_as_upsert => true}),
      cats:request(60000),
      cats:unit( elastic_put_return(_) )
   ].

%%
%%
elastic_lookup(Uri, Query) ->
   [m_http ||
      cats:new(Uri),
      cats:method('POST'),
      cats:header('Content-Type', "application/json"),
      cats:header('Transfer-Encoding', "chunked"),
      cats:header('Connection', 'keep-alive'),
      cats:payload(Query),
      cats:request(60000),
      cats:unit( elastic_lookup_return(_) )
   ].

elastic_lookup_return([{200, _, _}, Json]) ->
   {ok, Json};
elastic_lookup_return([{Code, _, _}, Reason]) ->
   {error, {Code, Reason}}.


%%
%%
identity_key(Key, Uri) ->
   [identity ||
      uri:segments(Uri),
      lens:put(lens:hd(), _, [undefined, <<"_doc">>, Key]),
      uri:segments(_, Uri)
   ].

identity_key(Uri) ->
   [identity ||
      uri:segments(Uri),
      lens:put(lens:hd(), _, [undefined, <<"_doc">>]),
      uri:segments(_, Uri)
   ].

%%
%%
identity_update(Key, Uri) ->
   [identity ||
      uri:segments(Uri),
      lens:put(lens:hd(), _, [undefined, <<"_doc">>, Key, <<"_update">>]),
      uri:segments(_, Uri)
   ].


%%
%%
identity_q(Uri, Op) ->
   [identity ||
      uri:segments(Uri),
      lens:put(lens:hd(), _, [undefined, Op]),
      uri:segments(_, Uri)
   ].

%%
identity_schema(Uri) ->
   [identity ||
      uri:segments(Uri),
      lens:put(lens:hd(), _, [undefined, <<"_mapping">>, <<"_doc">>]),
      uri:segments(_, Uri)   
   ].

