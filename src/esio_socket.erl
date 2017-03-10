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
         [$^ ||
            identity_key(Uri, Key),
            http_put(_, Val, Opts),
            http_run(_, State0)
         ]
      )
   };

handle({add, Val}, Pipe, #{uri := Uri, opts := Opts} = State0) ->
   {next_state, handle,
      http_return(Pipe, State0, 
         [$^ ||
            identity_type(Uri),
            http_add(_, Val, Opts),
            http_run(_, State0)
         ]
      )
   };

handle({get, Key}, Pipe, #{uri := Uri, opts := Opts} = State0) ->
   {next_state, handle,
      http_return(Pipe, State0,
         [$^ ||
            identity_key(Uri, Key),
            http_get(_, Opts),
            http_run(_, State0)
         ]
      )
   };


handle({remove, Key}, Pipe, #{uri := Uri, opts := Opts} = State0) ->
   {next_state, handle,
      http_return(Pipe, State0,
         [$^ ||
            identity_key(Uri, Key),
            http_remove(_, Opts),
            http_run(_, State0)
         ]
      )
   };

handle({lookup, Uid, Query}, Pipe, #{uri := Uri, opts := Opts} = State0) ->
   {next_state, handle,
      http_return(Pipe, State0,
         [$^ ||
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
   [$.||
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
      do([k_http ||
         _ /= new(Uri, Opts),
         _ /= x('PUT'),
         _ /= h('Content-Type', "application/json"),
         _ /= h('Transfer-Encoding', "chunked"),
         _ /= h('Connection', 'keep-alive'),
         _ /= d(Val),
         _ /= request(60000),
         _ =< http_put_return(_),
         return(_)
      ])
   }.

http_put_return([{Code, _, _, _}|Json])
 when Code =:= 201 orelse Code =:= 200 ->
   %% TODO: how to handle meta-data about key (e.g. created and version)?
   %%   <<"{\"_index\":\"a\",\"_type\":\"b\",\"_id\":\"1\",\"_version\":1,\"created\":true}">>
   case to_json(Json) of
      #{<<"_id">> := Id} -> 
         {ok, Id};
      _ -> 
         ok
   end;
http_put_return([{Code, _, _, _}|_]) ->
   {error, Code}.

%%
%%
http_add(Uri, Val, Opts) ->
   {ok,
      do([k_http ||
         _ /= new(Uri, Opts),
         _ /= x('POST'),
         _ /= h('Content-Type', "application/json"),
         _ /= h('Transfer-Encoding', "chunked"),
         _ /= h('Connection', 'keep-alive'),
         _ /= d(Val),
         _ /= request(60000),
         _ =< http_add_return(_),
         return(_)
      ])
   }.

http_add_return([{201, _, _, _}|Json]) ->
   %% TODO: how to handle meta-data about key (e.g. created and version)?
   %%   <<"{\"_index\":\"a\",\"_type\":\"b\",\"_id\":\"1\",\"_version\":1,\"created\":true}">>
   #{<<"_id">> := Id} = to_json(Json),
   {ok, Id};

http_add_return([{Code, _, _, _}|_]) ->
   {error, Code}.


%%
%%
http_get(Uri, Opts) ->
   {ok, 
      do([k_http ||
         _ /= new(Uri, Opts),
         _ /= x('GET'),
         _ /= h('Accept', "application/json"),
         _ /= h('Connection', 'keep-alive'),
         _ /= request(60000),
         _ =< http_get_return(_),
         return(_)
      ])
   }.

http_get_return([{200, _, _, _}|Json]) ->
   #{<<"_source">> := Val} = to_json(Json),
   {ok, Val};

http_get_return([{404, _, _, _}|_]) ->
   {error, not_found};

http_get_return([{Code, _, _, _}|_]) ->
   {error, Code}.

%%
%%
http_remove(Uri, Opts) ->
   {ok, 
      do([k_http ||
         _ /= new(Uri, Opts),
         _ /= x('DELETE'),
         _ /= h('Accept', "application/json"),
         _ /= h('Connection', 'keep-alive'),
         _ /= request(60000),
         _ =< http_remove_return(_),
         return(_)
      ])
   }.

http_remove_return([{Code, _, _, _}|_])
 when Code >= 200, Code < 300 orelse Code =:= 404 ->
   ok;
http_remove_return([{Code, _, _, _}|_]) ->
   {error, Code}.


%%
%%
http_lookup(Uri, Query, Opts) ->
   {ok,
      do([k_http ||
         _ /= new(Uri, Opts),
         _ /= x('POST'),
         _ /= h('Content-Type', "application/json"),
         _ /= h('Transfer-Encoding', "chunked"),
         _ /= h('Connection', 'keep-alive'),
         _ /= d(Query),
         _ /= request(60000),
         _ =< http_lookup_return(_),
         return(_)
      ])
   }.

http_lookup_return([{200, _, _, _}|Json]) ->
   #{<<"hits">> := Val} = to_json(Json),
   {ok, Val};

http_lookup_return([{Code, _, _, _}|_]) ->
   {error, Code}.


%%
%%
identity_key(Uri, {urn, undefined, Key}) ->
   case uri:segments(Uri) of
      undefined ->
         {ok, uri:s(uri:segments([Key], Uri))};
      [Cask, Type | _] -> 
         {ok, uri:s(uri:segments([Cask, Type, Key], Uri))};
      [Cask | _] -> 
         {ok, uri:s(uri:segments([Cask, Key], Uri))};
      [] ->
         {ok, uri:s(uri:segments([Key], Uri))}
   end;

identity_key(Uri, {urn, Type, Key}) ->
   case uri:segments(Uri) of
      undefined ->
         {ok, uri:s(uri:segments([Type, Key], Uri))};
      [Cask | _] -> 
         {ok, uri:s(uri:segments([Cask, Type, Key], Uri))};
      [] ->
         {ok, uri:s(uri:segments([Type, Key], Uri))}
   end.


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
identity_lookup(Uri, ?WILDCARD) ->
   case uri:segments(Uri) of
      [Cask] ->
         {ok, uri:s(uri:segments([Cask, <<"_search">>], Uri))};
      [Cask, Type|_] ->
         {ok, uri:s(uri:segments([Cask, Type, <<"_search">>], Uri))};
      Path ->
         {error, {badkey, Path}}
   end;

identity_lookup(Uri, {urn, Type, _}) ->
   case uri:segments(Uri) of
      [Cask|_] ->
         {ok, uri:s(uri:segments([Cask, Type, <<"_search">>], Uri))};
      Path ->
         {error, {badkey, join(Path, Type)}}
   end.

%%
%%
join(undefined, B) ->
   [B];
join(A, B) ->
   A ++ [B].

