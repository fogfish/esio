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
   schema/1,
   put_cask_url/1, put_type_url/1,
   add_cask_url/1, add_type_url/1,
   get_cask_url/1, get_type_url/1,
   rem_cask_url/1, rem_type_url/1,
   lookup/1, stream/1
]).

%%%----------------------------------------------------------------------------   
%%%
%%% factory
%%%
%%%----------------------------------------------------------------------------   

all() ->
   [
      {group, keyval}
     ,{group, stream}
   ].

groups() ->
   [
      {keyval, [parallel], [
         schema, 
         put_cask_url, put_type_url,
         add_cask_url, add_type_url,
         get_cask_url, get_type_url,
         rem_cask_url, rem_type_url
      ]},
      {stream, [parallel], [
         lookup, stream
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
   lager:set_loglevel(lager_console_backend, debug),
   [
      {elastic_url_base, "http://127.0.0.1:9200"}
     ,{elastic_url_cask, "http://127.0.0.1:9200/testcask"}
     ,{elastic_url_type, "http://127.0.0.1:9200/testcask/testtype"}
     |Config
   ].

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

%%
%%
schema(Config) ->
   {ok, Sock} = esio:socket( ?config(elastic_url_base, Config) ),
   ok = esio:put(Sock, <<"/a">>, #{}),
   ok = esio:remove(Sock, <<"/a">>),

   {error, {badkey, _}} = esio:put(Sock, <<"xxx">>, #{}),
   {error, {badkey, _}} = esio:put(Sock, {urn, <<"xxx">>, <<"xxx">>}, #{}),

   esio:close(Sock).
      
%%
%%
put_cask_url(Config) ->
   {ok, Sock} = esio:socket( ?config(elastic_url_cask, Config) ),

   Key001 = uri:new(<<"urn:testcask:001">>),   
   {ok, <<"001">>} = esio:put(Sock, Key001, #{<<"val">> => <<"001">>}),

   Key002 = uri:new(<<"urn:testcask:002">>),
   ok = esio:put_(Sock, Key002, #{<<"val">> => <<"002">>}, false),

   Key003 = uri:new(<<"urn:testcask:003">>),
   {ok, <<"003">>} = recv(esio:put_(Sock, Key003, #{<<"val">> => <<"003">>}, true)),

   {error, {badkey, _}} = esio:put(Sock, <<"xxx">>, #{<<"val">> => <<"xxx">>}),

   esio:close(Sock).

%%
%%
put_type_url(Config) ->
   {ok, Sock} = esio:socket( ?config(elastic_url_type, Config) ),

   Key004 = uri:new(<<"urn:testcask:004">>),   
   {ok, <<"004">>} = esio:put(Sock, Key004, #{<<"val">> => <<"004">>}),

   Key005 = uri:new(<<"urn:testcask:005">>),
   ok = esio:put_(Sock, Key005, #{<<"val">> => <<"005">>}, false),

   Key006 = uri:new(<<"urn:testcask:006">>),
   {ok, <<"006">>} = recv(esio:put_(Sock, Key006, #{<<"val">> => <<"006">>}, true)),

   Key007 = <<"007">>,   
   {ok, <<"007">>} = esio:put(Sock, Key007, #{<<"val">> => <<"007">>}),

   Key008 = <<"008">>,
   ok = esio:put_(Sock, Key008, #{<<"val">> => <<"008">>}, false),

   Key009 = <<"009">>,
   {ok, <<"009">>} = recv(esio:put_(Sock, Key009, #{<<"val">> => <<"009">>}, true)),

   esio:close(Sock).

%%
%%
add_cask_url(Config) ->
   {ok, Sock} = esio:socket( ?config(elastic_url_cask, Config) ),

   {error, _} = esio:add(Sock, #{<<"val">> => <<"010">>}),

   ok = esio:add_(Sock, #{<<"val">> => <<"011">>}, false),

   {error, _} = recv(esio:add_(Sock, #{<<"val">> => <<"012">>}, true)),

   esio:close(Sock).


%%
%%
add_type_url(Config) ->
   {ok, Sock} = esio:socket( ?config(elastic_url_type, Config) ),

   {ok, _} = esio:add(Sock, #{<<"val">> => <<"013">>}),

   ok = esio:add_(Sock, #{<<"val">> => <<"014">>}, false),

   {ok, _} = recv(esio:add_(Sock, #{<<"val">> => <<"015">>}, true)),

   esio:close(Sock).


%%
%%
get_cask_url(Config) ->
   {ok, Sock} = esio:socket( ?config(elastic_url_cask, Config) ),

   {error, not_found} = esio:get(Sock, uri:new(<<"urn:testcask:xxx">>)),

   Key016 = uri:new(<<"urn:testcask:016">>),   
   {ok, <<"016">>} = esio:put(Sock, Key016, #{<<"val">> => <<"016">>}),
   {ok, #{<<"val">> := <<"016">>}} = esio:get(Sock, Key016),

   Key017 = uri:new(<<"urn:testcask:017">>),
   ok = esio:put_(Sock, Key017, #{<<"val">> => <<"017">>}, false),
   {ok, #{<<"val">> := <<"017">>}} = esio:get(Sock, Key017),

   Key018 = uri:new(<<"urn:testcask:018">>),
   {ok, <<"018">>} = recv(esio:put_(Sock, Key018, #{<<"val">> => <<"018">>}, true)),
   {ok, #{<<"val">> := <<"018">>}} = esio:get(Sock, Key018),

   esio:close(Sock).

%%
%%
get_type_url(Config) ->
   {ok, Sock} = esio:socket( ?config(elastic_url_type, Config) ),

   {error, not_found} = esio:get(Sock, <<"xxx">>),

   Key019 = <<"019">>,   
   {ok, <<"019">>} = esio:put(Sock, Key019, #{<<"val">> => <<"019">>}),
   {ok, #{<<"val">> := <<"019">>}} = esio:get(Sock, Key019),

   Key020 = <<"020">>,
   ok = esio:put_(Sock, Key020, #{<<"val">> => <<"020">>}, false),
   {ok, #{<<"val">> := <<"020">>}} = esio:get(Sock, Key020),

   Key021 = <<"021">>,
   {ok, <<"021">>} = recv(esio:put_(Sock, Key021, #{<<"val">> => <<"021">>}, true)),
   {ok, #{<<"val">> := <<"021">>}} = esio:get(Sock, Key021),

   esio:close(Sock).


%%
%%
rem_cask_url(Config) ->
   {ok, Sock} = esio:socket( ?config(elastic_url_cask, Config) ),

   Key022 = uri:new(<<"urn:testcask:022">>),   
   {ok, <<"022">>} = esio:put(Sock, Key022, #{<<"val">> => <<"022">>}),
   ok = esio:remove(Sock, Key022),
   {error, not_found} = esio:get(Sock, Key022),

   Key023 = uri:new(<<"urn:testcask:023">>),
   ok = esio:put_(Sock, Key023, #{<<"val">> => <<"023">>}, false),
   ok = esio:remove(Sock, Key023),
   {error, not_found} = esio:get(Sock, Key023),

   Key024 = uri:new(<<"urn:testcask:024">>),
   {ok, <<"024">>} = recv(esio:put_(Sock, Key024, #{<<"val">> => <<"024">>}, true)),
   ok = esio:remove(Sock, Key024),
   {error, not_found} = esio:get(Sock, Key024),

   esio:close(Sock).

%%
%%
rem_type_url(Config) ->
   {ok, Sock} = esio:socket( ?config(elastic_url_type, Config) ),

   Key025 = <<"025">>,   
   {ok, <<"025">>} = esio:put(Sock, Key025, #{<<"val">> => <<"025">>}),
   ok = esio:remove(Sock, Key025),
   {error, not_found} = esio:get(Sock, Key025),

   Key026 = <<"026">>,
   ok = esio:put_(Sock, Key026, #{<<"val">> => <<"026">>}, false),
   ok = esio:remove(Sock, Key026),
   {error, not_found} = esio:get(Sock, Key026),

   Key027 = <<"027">>,
   {ok, <<"027">>} = recv(esio:put_(Sock, Key027, #{<<"val">> => <<"027">>}, true)),
   ok = esio:remove(Sock, Key027),
   {error, not_found} = esio:get(Sock, Key027),

   esio:close(Sock).


lookup(Config) ->
   {ok, Sock} = esio:socket( ?config(elastic_url_cask, Config) ),

   {ok, _} = esio:lookup(Sock, #{'query' => #{'match_all' => #{}} }),

   esio:close(Sock).


stream(Config) ->
   {ok, Sock} = esio:socket( ?config(elastic_url_cask, Config) ),

   Stream  = esio:stream(Sock, #{'query' => #{'match_all' => #{}} }),
   [_ | _] = stream:list(Stream),

   esio:close(Sock).


%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%%
recv(Ref) ->
   receive
      {Ref, Msg} ->
         Msg
   after 5000 ->
      exit(timeout)
   end.


