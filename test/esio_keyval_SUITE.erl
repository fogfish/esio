%%
%%   Copyright (C) 2016 Zalando SE
%%
%%   This software may be modified and distributed under the terms
%%   of the MIT license.  See the LICENSE file for details.
%%
-module(esio_keyval_SUITE).
-author('dmitry.kolesnikov@zalando.fi').

-include_lib("common_test/include/ct.hrl").

%% common test
-export([
   all/0 ,groups/0
  ,init_per_suite/1 ,end_per_suite/1
  ,init_per_group/2 ,end_per_group/2
]).
-export([
   put/1, get/1, remove/1, lookup/1
]).
-export([
   stream/1
]).

%%%----------------------------------------------------------------------------   
%%%
%%% factory
%%%
%%%----------------------------------------------------------------------------   

all() ->
   [
      {group, keyval},
      {group, stream}
   ].

groups() ->
   [
      {keyval, [parallel], [
         put, get, remove, lookup
      ]},
      {stream, [parallel], [
         stream
      ]}
   ].


%%%----------------------------------------------------------------------------   
%%%
%%% init
%%%
%%%----------------------------------------------------------------------------   

%%
init_per_suite(Config) ->
   ok = esio:start(),
   [{elastic_search_url, "http://docker:9200"},{base, uri:new("urn:es:testcask:testtype")}|Config].

end_per_suite(_Config) ->
   ok.

%%
init_per_group(_, Config) ->
   Config.

end_per_group(_, _Config) ->
   ok.


%%%----------------------------------------------------------------------------   
%%%
%%% unit test
%%%
%%%----------------------------------------------------------------------------   

put(Config) ->
   {ok, Sock} = esio:socket( ?config(elastic_search_url, Config) ),
   Key1 = uri:join([put1], ?config(base, Config)),
   ok   = esio:put(Sock, Key1, #{<<"val">> => 1}),

   timer:sleep(100),
   Key2 = uri:join([put2], ?config(base, Config)),
   ok   = esio:put_(Sock, Key2, #{<<"val">> => 2}, false),

   timer:sleep(100),
   Key3 = uri:join([put3], ?config(base, Config)),
   recv(ok, 
      esio:put_(Sock, Key3, #{<<"val">> => 3}, true)
   ),
   esio:close(Sock).


get(Config) ->
   {ok, Sock} = esio:socket( ?config(elastic_search_url, Config) ),
   Key1 = uri:join([get1], ?config(base, Config)),
   ok   = esio:put(Sock, Key1, #{<<"val">> => 1}),
   {ok, #{<<"val">> := 1}} = esio:get(Sock, Key1),

   timer:sleep(100), 
   Key2 = uri:join([get2], ?config(base, Config)),
   ok   = esio:put(Sock, Key2, #{<<"val">> => 1}),
   ok   = esio:get_(Sock, Key2, false),
  
   timer:sleep(100),
   Key3 = uri:join([get3], ?config(base, Config)),
   ok   = esio:put(Sock, Key3, #{<<"val">> => 1}),
   recv({ok, #{<<"val">> => 1}},
      esio:get_(Sock, Key3, true)
   ),

   esio:close(Sock).


remove(Config) ->
   {ok, Sock} = esio:socket( ?config(elastic_search_url, Config) ),
   Key1 = uri:join([rem1], ?config(base, Config)),
   ok   = esio:put(Sock, Key1, #{<<"val">> => 1}),
   ok   = esio:remove(Sock, Key1),
   {error, not_found} = esio:get(Sock, Key1),

   Key2 = uri:join([rem2], ?config(base, Config)),
   ok   = esio:put(Sock, Key2, #{<<"val">> => 1}),
   ok   = esio:remove_(Sock, Key2, false),
   {error, not_found} = esio:get(Sock, Key2),

   %%
   %% Note: looks like Linearizability issue at Elastic Search
   %% DELETE / GET are not handled properly
   Key3 = uri:join([rem3], ?config(base, Config)),
   ok   = esio:put(Sock, Key3, #{<<"val">> => 1}),
   recv(ok,
      esio:remove_(Sock, Key3, true)
   ),
   {error, not_found} = esio:get(Sock, Key3),

   esio:close(Sock).

lookup(Config) ->
   {ok, Sock} = esio:socket( ?config(elastic_search_url, Config) ),
   Urn = ?config(base, Config),

   {ok, _} = esio:lookup(Sock, Urn, #{'query' => #{'match_all' => #{}} }),

   esio:close(Sock).


stream(Config) ->
   {ok, Sock} = esio:socket( ?config(elastic_search_url, Config) ),
   Urn = ?config(base, Config),

   Stream  = esio:stream(Sock, Urn, #{'query' => #{'match_all' => #{}} }),
   [_ | _] = stream:list(Stream),

   esio:close(Sock).


%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

recv(Msg, Ref) ->
   receive
      {Ref, Msg} ->
         ok
   after 5000 ->
      exit(timeout)
   end.


