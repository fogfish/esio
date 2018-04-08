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
   [200 | State] = http( elastic_ping(Uri, Opts) ),   
   {ok, handle, State#{uri => Uri}}.

free(_, _) ->
   ok.

%%-----------------------------------------------------------------------------
%%
%% state machine
%%
%%-----------------------------------------------------------------------------

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

handle({remove, Key}, Pipe, #{uri := Uri} = State) ->
   {next_state, handle,
      [identity ||
         identity_key(Key, Uri),
         elastic_remove(_),
         http(_, State),
         ack(Pipe, _)
      ]
   };

handle({lookup, Query}, Pipe, #{uri := Uri} = State) ->
   {next_state, handle,
      [identity ||
         identity_q(Uri),
         elastic_lookup(_, Query),
         http(_, State),
         ack(Pipe, _)
      ]
   };

handle({http, _, passive}, Pipe, State) ->
   pipe:a(Pipe, {active, 1024}),
   {next_state, handle, State};

handle(close, _, State) ->
   {stop, normal, State}.


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

elastic_lookup_return([{200, _, _}, #{<<"hits">> := Val}]) ->
   {ok, Val};
elastic_lookup_return([{200, _, _}, #{<<"deleted">> := _} = Val]) ->
   {ok, Val};
elastic_lookup_return([{Code, _, _}, Reason]) ->
   {error, {Code, Reason}}.





% http_put_return([{Code, _, _}|Json])
%  when Code =:= 201 orelse Code =:= 200 ->
%    %% TODO: how to handle meta-data about key (e.g. created and version)?
%    %%   <<"{\"_index\":\"a\",\"_type\":\"b\",\"_id\":\"1\",\"_version\":1,\"created\":true}">>
%    case to_json(Json) of
%       #{<<"_id">> := Id} -> 
%          {ok, Id};
%       _ -> 
%          ok
%    end;
% http_put_return([{Code, _, _}|_]) ->
%    {error, Code}.




%%
%%
% to_json(Pckt) ->
%    [identity ||
%       erlang:iolist_to_binary(Pckt),
%       jsx:decode(_, [return_maps])
%    ].

%%
%%
% http_run(Http, State) ->
%    {ok, Http(State)}.

%%
%%
% http_return(Pipe, _, {ok, [Result | State]}) -> 
%    pipe:a(Pipe, Result),
%    State;

% http_return(Pipe, State, {error, _} = Error) ->
%    pipe:a(Pipe, Error),
%    State.


%%
%%
% http_put(Uri, Val, Opts) ->
%    {ok, 
%       [m_http ||
%          cats:new(Uri, Opts),
%          cats:x('PUT'),
%          cats:h('Content-Type', "application/json"),
%          cats:h('Transfer-Encoding', "chunked"),
%          cats:h('Connection', 'keep-alive'),
%          cats:d(Val),
%          cats:request(60000),
%          cats:unit(http_put_return(_))
%       ]
%    }.

% http_put_return([{Code, _, _}|Json])
%  when Code =:= 201 orelse Code =:= 200 ->
%    %% TODO: how to handle meta-data about key (e.g. created and version)?
%    %%   <<"{\"_index\":\"a\",\"_type\":\"b\",\"_id\":\"1\",\"_version\":1,\"created\":true}">>
%    case to_json(Json) of
%       #{<<"_id">> := Id} -> 
%          {ok, Id};
%       _ -> 
%          ok
%    end;
% http_put_return([{Code, _, _}|_]) ->
%    {error, Code}.

%%
%%
% http_add(Uri, Val, Opts) ->
%    {ok,
%       [m_http ||
%          cats:new(Uri, Opts),
%          cats:x('POST'),
%          cats:h('Content-Type', "application/json"),
%          cats:h('Transfer-Encoding', "chunked"),
%          cats:h('Connection', 'keep-alive'),
%          cats:d(Val),
%          cats:request(60000),
%          cats:unit(http_add_return(_))
%       ]
%    }.

% http_add_return([{201, _, _}|Json]) ->
%    %% TODO: how to handle meta-data about key (e.g. created and version)?
%    %%   <<"{\"_index\":\"a\",\"_type\":\"b\",\"_id\":\"1\",\"_version\":1,\"created\":true}">>
%    #{<<"_id">> := Id} = to_json(Json),
%    {ok, Id};

% http_add_return([{Code, _, _}|_]) ->
%    {error, Code}.


%%
%%
% http_get(Uri, Opts) ->
%    {ok, 
%       [m_http ||
%          cats:new(Uri, Opts),
%          cats:x('GET'),
%          cats:h('Accept', "application/json"),
%          cats:h('Connection', 'keep-alive'),
%          cats:request(60000),
%          cats:unit(http_get_return(_))
%       ]
%    }.

% http_get_return([{200, _, _}|Json]) ->
%    #{<<"_source">> := Val} = to_json(Json),
%    {ok, Val};

% http_get_return([{404, _, _}|_]) ->
%    {error, not_found};

% http_get_return([{Code, _, _}|_]) ->
%    {error, Code}.

%%
%%
% http_remove(Uri, Opts) ->
%    {ok, 
%       [m_http ||
%          cats:new(Uri, Opts),
%          cats:x('DELETE'),
%          cats:h('Accept', "application/json"),
%          cats:h('Connection', 'keep-alive'),
%          cats:request(60000),
%          cats:unit(http_remove_return(_))
%       ]
%    }.

% http_remove_return([{Code, _, _}|_])
%  when Code >= 200, Code < 300 orelse Code =:= 404 ->
%    ok;
% http_remove_return([{Code, _, _}|_]) ->
%    {error, Code}.


%%
%%
% http_lookup(Uri, Query, Opts) ->
%    {ok,
%       [m_http ||
%          cats:new(Uri, Opts),
%          cats:x('POST'),
%          cats:h('Content-Type', "application/json"),
%          cats:h('Transfer-Encoding', "chunked"),
%          cats:h('Connection', 'keep-alive'),
%          cats:d(Query),
%          cats:request(60000),
%          cats:unit(http_lookup_return(_))
%       ]
%    }.

% http_lookup_return([{200, _, _}|Json]) ->
%    case to_json(Json) of
%       %% search results
%       #{<<"hits">> := Val} ->
%          {ok, Val};
%       %% delete by query results
%       #{<<"deleted">> := _} = Val ->
%          {ok, Val}
%    end;

% http_lookup_return([{Code, _, _}|_]) ->
%    {error, Code}.

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
identity_q(Uri) ->
   [identity ||
      uri:segments(Uri),
      lens:put(lens:hd(), _, [undefined, <<"_search">>]),
      uri:segments(_, Uri)
   ].



%%
%%
% identity_key(Uri, Key) ->
%    identity_key(segments(Uri), Uri, Key).

% identity_key([], Uri, {urn, undefined, <<$/, _/binary>> = Key}) ->
%    % socket is not bound to index, ONLY absolute key is accepted
%    {ok, uri:s(uri:path(Key, Uri))};

% identity_key([Cask], Uri, {urn, Type, Key})
%  when Type =/= undefined ->
%    % socket is bound to index, ONLY `urn()` key is supported
%    {ok, uri:s(uri:segments([Cask, Type, Key], Uri))};

% identity_key([Cask, Type], Uri, {urn, undefined, Key}) ->
%    % socket is bound to index and type
%    {ok, uri:s(uri:segments([Cask, Type, Key], Uri))};

% identity_key([Cask, _], Uri, {urn, Type, Key}) ->
%    % socket is bound to index and type but `urn()` format of key overrides it
%    {ok, uri:s(uri:segments([Cask, Type, Key], Uri))};

% identity_key(_, _, Key) ->
%    {error, {badkey, Key}}.

%%
%%
% identity_type(Uri) ->
%    case uri:segments(Uri) of
%       [Cask, Type | _] -> 
%          {ok, uri:s(uri:segments([Cask, Type], Uri))};
%       Path -> 
%          {error, {badkey, Path}}
%    end.

%%
%%
% identity_lookup(Uri, Key) ->
%    identity_lookup(segments(Uri), Uri, Key).

% identity_lookup([], Uri,  {urn, _, <<"_search">>}) ->
%    {ok, uri:s(uri:segments([<<"_search">>], Uri))};
% identity_lookup([], Uri, {urn, Cask, Type}) ->
%    {ok, uri:s(uri:segments([Cask, Type, <<"_search">>], Uri))};
 
% identity_lookup([Cask], Uri, {urn, _, <<"_search">>}) ->
%    {ok, uri:s(uri:segments([Cask, <<"_search">>], Uri))};
% identity_lookup([Cask], Uri, {urn, _, <<"_delete_by_query">>}) ->
%    {ok, uri:s(uri:segments([Cask, <<"_delete_by_query">>], Uri))};

% identity_lookup([Cask], Uri, {urn, _, Type}) ->
%    {ok, uri:s(uri:segments([Cask, Type, <<"_search">>], Uri))};
% identity_lookup([Cask, Type], Uri, {urn, _, <<"_delete_by_query">>}) ->
%    {ok, uri:s(uri:segments([Cask, Type, <<"_delete_by_query">>], Uri))};   
% identity_lookup([Cask, Type], Uri, _) ->
%    {ok, uri:s(uri:segments([Cask, Type, <<"_search">>], Uri))};
% identity_lookup(_, _, Key) ->
%    {error, {badkey, Key}}.


% %%
% %%
% segments(Uri) ->
%    case uri:segments(Uri) of
%       undefined ->
%          [];
%       Segments ->
%          Segments 
%    end.
