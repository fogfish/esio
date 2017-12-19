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
   {ok, handle, 
      #{
         uri  => Uri, 
         opts => Opts
      }
   }.

free(_, _) ->
   ok.

%%-----------------------------------------------------------------------------
%%
%% state machine
%%
%%-----------------------------------------------------------------------------

handle({put, Key, Val}, Pipe, #{uri := Uri, opts := Opts} = State0) ->
   {next_state, handle,
      http_return(Pipe, State0,
         [either ||
            identity_key(Uri, Key),
            http_put(_, Val, Opts),
            http_run(_, State0)
         ]
      )
   };

handle({add, Val}, Pipe, #{uri := Uri, opts := Opts} = State0) ->
   {next_state, handle,
      http_return(Pipe, State0, 
         [either ||
            identity_type(Uri),
            http_add(_, Val, Opts),
            http_run(_, State0)
         ]
      )
   };

handle({get, Key}, Pipe, #{uri := Uri, opts := Opts} = State0) ->
   {next_state, handle,
      http_return(Pipe, State0,
         [either ||
            identity_key(Uri, Key),
            http_get(_, Opts),
            http_run(_, State0)
         ]
      )
   };


handle({remove, Key}, Pipe, #{uri := Uri, opts := Opts} = State0) ->
   {next_state, handle,
      http_return(Pipe, State0,
         [either ||
            identity_key(Uri, Key),
            http_remove(_, Opts),
            http_run(_, State0)
         ]
      )
   };

handle({lookup, Uid, Query}, Pipe, #{uri := Uri, opts := Opts} = State0) ->
   {next_state, handle,
      http_return(Pipe, State0,
         [either ||
            identity_lookup(Uri, Uid),
            http_lookup(_, Query, Opts),
            http_run(_, State0)
         ]
      )
   };

handle(close, _, State) ->
   {stop, normal, State}.


%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%%
to_json(Pckt) ->
   [identity ||
      erlang:iolist_to_binary(Pckt),
      jsx:decode(_, [return_maps])
   ].

%%
%%
http_run(Http, State) ->
   {ok, Http(State)}.

%%
%%
http_return(Pipe, _, {ok, [Result | State]}) -> 
   pipe:a(Pipe, Result),
   State;

http_return(Pipe, State, {error, _} = Error) ->
   pipe:a(Pipe, Error),
   State.


%%
%%
http_put(Uri, Val, Opts) ->
   {ok, 
      [m_http ||
         cats:new(Uri, Opts),
         cats:x('PUT'),
         cats:h('Content-Type', "application/json"),
         cats:h('Transfer-Encoding', "chunked"),
         cats:h('Connection', 'keep-alive'),
         cats:d(Val),
         cats:request(60000),
         cats:unit(http_put_return(_))
      ]
   }.

http_put_return([{Code, _, _}|Json])
 when Code =:= 201 orelse Code =:= 200 ->
   %% TODO: how to handle meta-data about key (e.g. created and version)?
   %%   <<"{\"_index\":\"a\",\"_type\":\"b\",\"_id\":\"1\",\"_version\":1,\"created\":true}">>
   case to_json(Json) of
      #{<<"_id">> := Id} -> 
         {ok, Id};
      _ -> 
         ok
   end;
http_put_return([{Code, _, _}|_]) ->
   {error, Code}.

%%
%%
http_add(Uri, Val, Opts) ->
   {ok,
      [m_http ||
         cats:new(Uri, Opts),
         cats:x('POST'),
         cats:h('Content-Type', "application/json"),
         cats:h('Transfer-Encoding', "chunked"),
         cats:h('Connection', 'keep-alive'),
         cats:d(Val),
         cats:request(60000),
         cats:unit(http_add_return(_))
      ]
   }.

http_add_return([{201, _, _}|Json]) ->
   %% TODO: how to handle meta-data about key (e.g. created and version)?
   %%   <<"{\"_index\":\"a\",\"_type\":\"b\",\"_id\":\"1\",\"_version\":1,\"created\":true}">>
   #{<<"_id">> := Id} = to_json(Json),
   {ok, Id};

http_add_return([{Code, _, _}|_]) ->
   {error, Code}.


%%
%%
http_get(Uri, Opts) ->
   {ok, 
      [m_http ||
         cats:new(Uri, Opts),
         cats:x('GET'),
         cats:h('Accept', "application/json"),
         cats:h('Connection', 'keep-alive'),
         cats:request(60000),
         cats:unit(http_get_return(_))
      ]
   }.

http_get_return([{200, _, _}|Json]) ->
   #{<<"_source">> := Val} = to_json(Json),
   {ok, Val};

http_get_return([{404, _, _}|_]) ->
   {error, not_found};

http_get_return([{Code, _, _}|_]) ->
   {error, Code}.

%%
%%
http_remove(Uri, Opts) ->
   {ok, 
      [m_http ||
         cats:new(Uri, Opts),
         cats:x('DELETE'),
         cats:h('Accept', "application/json"),
         cats:h('Connection', 'keep-alive'),
         cats:request(60000),
         cats:unit(http_remove_return(_))
      ]
   }.

http_remove_return([{Code, _, _}|_])
 when Code >= 200, Code < 300 orelse Code =:= 404 ->
   ok;
http_remove_return([{Code, _, _}|_]) ->
   {error, Code}.


%%
%%
http_lookup(Uri, Query, Opts) ->
   {ok,
      [m_http ||
         cats:new(Uri, Opts),
         cats:x('POST'),
         cats:h('Content-Type', "application/json"),
         cats:h('Transfer-Encoding', "chunked"),
         cats:h('Connection', 'keep-alive'),
         cats:d(Query),
         cats:request(60000),
         cats:unit(http_lookup_return(_))
      ]
   }.

http_lookup_return([{200, _, _}|Json]) ->
   case to_json(Json) of
      %% search results
      #{<<"hits">> := Val} ->
         {ok, Val};
      %% delete by query results
      #{<<"deleted">> := _} = Val ->
         {ok, Val}
   end;

http_lookup_return([{Code, _, _}|_]) ->
   {error, Code}.


%%
%%
identity_key(Uri, Key) ->
   identity_key(segments(Uri), Uri, Key).

identity_key([], Uri, {urn, undefined, <<$/, _/binary>> = Key}) ->
   % socket is not bound to index, ONLY absolute key is accepted
   {ok, uri:s(uri:path(Key, Uri))};

identity_key([Cask], Uri, {urn, Type, Key})
 when Type =/= undefined ->
   % socket is bound to index, ONLY `urn()` key is supported
   {ok, uri:s(uri:segments([Cask, Type, Key], Uri))};

identity_key([Cask, Type], Uri, {urn, undefined, Key}) ->
   % socket is bound to index and type
   {ok, uri:s(uri:segments([Cask, Type, Key], Uri))};

identity_key([Cask, _], Uri, {urn, Type, Key}) ->
   % socket is bound to index and type but `urn()` format of key overrides it
   {ok, uri:s(uri:segments([Cask, Type, Key], Uri))};

identity_key(_, _, Key) ->
   {error, {badkey, Key}}.

%%
%%
identity_type(Uri) ->
   case uri:segments(Uri) of
      [Cask, Type | _] -> 
         {ok, uri:s(uri:segments([Cask, Type], Uri))};
      Path -> 
         {error, {badkey, Path}}
   end.

%%
%%
identity_lookup(Uri, Key) ->
   identity_lookup(segments(Uri), Uri, Key).

identity_lookup([], Uri,  {urn, _, <<"_search">>}) ->
   {ok, uri:s(uri:segments([<<"_search">>], Uri))};
identity_lookup([], Uri, {urn, Cask, Type}) ->
   {ok, uri:s(uri:segments([Cask, Type, <<"_search">>], Uri))};
 
identity_lookup([Cask], Uri, {urn, _, <<"_search">>}) ->
   {ok, uri:s(uri:segments([Cask, <<"_search">>], Uri))};
identity_lookup([Cask], Uri, {urn, _, <<"_delete_by_query">>}) ->
   {ok, uri:s(uri:segments([Cask, <<"_delete_by_query">>], Uri))};

identity_lookup([Cask], Uri, {urn, _, Type}) ->
   {ok, uri:s(uri:segments([Cask, Type, <<"_search">>], Uri))};
identity_lookup([Cask, Type], Uri, {urn, _, <<"_delete_by_query">>}) ->
   {ok, uri:s(uri:segments([Cask, Type, <<"_delete_by_query">>], Uri))};   
identity_lookup([Cask, Type], Uri, _) ->
   {ok, uri:s(uri:segments([Cask, Type, <<"_search">>], Uri))};
identity_lookup(_, _, Key) ->
   {error, {badkey, Key}}.


%%
%%
segments(Uri) ->
   case uri:segments(Uri) of
      undefined ->
         [];
      Segments ->
         Segments 
   end.
