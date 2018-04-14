%%
%%   Copyright (C) 2018 Dmitry Kolesnikov
%%
%%   This software may be modified and distributed under the terms
%%   of the MIT license.  See the LICENSE file for details.
%%
-module(esio_SUITE).
-compile({parse_transform, category}).

-include_lib("common_test/include/ct.hrl").

%% common test
-export([
   all/0,
   groups/0,
   init_per_suite/1,
   end_per_suite/1,
   init_per_group/2,
   end_per_group/2,
   init_per_testcase/2,
   end_per_testcase/2
]).
-export([
   socket/1,
   schema/1,
   put_call/1,
   put_cast/1,
   put_send/1,
   add_call/1,
   add_cast/1,
   add_send/1,
   get_call/1,
   get_cast/1,
   get_send/1,
   remove_call/1,
   remove_cast/1,
   remove_send/1,
   update_call/1,
   update_cast/1,
   update_send/1,   
   lookup_call/1,
   lookup_cast/1,
   lookup_send/1,
   stream/1,
   match/1,
   bulk/1,
   bulk_sync/1,
   bulk_timeout/1
]).

%%%----------------------------------------------------------------------------   
%%%
%%% suite
%%%
%%%----------------------------------------------------------------------------   
all() ->
   [
      {group, interface}
   ].

groups() ->
   [
      {interface, [], 
         [socket, schema, put_call, put_cast, put_send, add_call, add_cast, add_send, 
         get_call, get_cast, get_send, remove_call, remove_cast, remove_send, 
         update_call, update_cast, update_send, 
         lookup_call, lookup_cast, lookup_send, stream, match,
         bulk, bulk_sync, bulk_timeout]}
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% init
%%%
%%%----------------------------------------------------------------------------   
init_per_suite(Config) ->
   esio:start(),
   Config.

end_per_suite(_Config) ->
   application:stop(esio),
   ok.

%% 
%%
init_per_group(_, Config) ->
   Config.

end_per_group(_, _Config) ->
   ok.

%%
init_per_testcase(_, Config) ->
   meck:new(m_http, [passthrough]),
   Config.

end_per_testcase(_, _Config) ->
   meck:unload(m_http),
   ok.


%%%----------------------------------------------------------------------------   
%%%
%%% unit tests
%%%
%%%---------------------------------------------------------------------------- 

%%
socket(_Config) ->
   meck:expect(m_http, request, elastic_ping()),

   {ok, Sock} = esio:socket("http://localhost:9200/bucket"),
   ok = esio:close(Sock),

   true = meck:validate(m_http).

%%
schema(_Config) ->
   meck:expect(m_http, request, elastic_mock(esio_FIXTURE:elastic_ret_schema())),
   Expect = esio_FIXTURE:bucket(),

   {ok, Sock} = esio:socket("http://localhost:9200/bucket", []),
   {ok, Expect} = esio:schema(Sock, #{}),
   ok = esio:close(Sock),

   true = meck:validate(m_http).


%%
put_call(_Config) ->
   meck:expect(m_http, request, elastic_mock(esio_FIXTURE:elastic_ret_put())),
   Expect = esio_FIXTURE:key(),

   {ok, Sock} = esio:socket("http://localhost:9200/bucket", []),
   {ok, Expect} = esio:put(Sock, esio_FIXTURE:key(), esio_FIXTURE:doc()),
   ok = esio:close(Sock),

   true = meck:validate(m_http).

%%
put_cast(_Config) ->
   meck:expect(m_http, request, elastic_mock(esio_FIXTURE:elastic_ret_put())),
   Expect = esio_FIXTURE:key(),

   {ok, Sock} = esio:socket("http://localhost:9200/bucket", []),
   Ref = esio:put_(Sock, esio_FIXTURE:key(), esio_FIXTURE:doc(), true),
   receive {Ref, {ok, Expect}} -> ok end,
   ok = esio:close(Sock),

   true = meck:validate(m_http).

%%
put_send(_Config) ->
   meck:expect(m_http, request, elastic_mock(esio_FIXTURE:elastic_ret_put())),

   {ok, Sock} = esio:socket("http://localhost:9200/bucket", []),
   ok = esio:put_(Sock, esio_FIXTURE:key(), esio_FIXTURE:doc()),
   ok = esio:close(Sock),

   true = meck:validate(m_http).


%%
add_call(_Config) ->
   meck:expect(m_http, request, elastic_mock(esio_FIXTURE:elastic_ret_put())),
   Expect = esio_FIXTURE:key(),

   {ok, Sock} = esio:socket("http://localhost:9200/bucket", []),
   {ok, Expect} = esio:add(Sock, esio_FIXTURE:doc()),
   ok = esio:close(Sock),

   true = meck:validate(m_http).

%%
add_cast(_Config) ->
   meck:expect(m_http, request, elastic_mock(esio_FIXTURE:elastic_ret_put())),
   Expect = esio_FIXTURE:key(),

   {ok, Sock} = esio:socket("http://localhost:9200/bucket", []),
   Ref = esio:add_(Sock, esio_FIXTURE:doc(), true),
   receive {Ref, {ok, Expect}} -> ok end,
   ok = esio:close(Sock),

   true = meck:validate(m_http).

%%
add_send(_Config) ->
   meck:expect(m_http, request, elastic_mock(esio_FIXTURE:elastic_ret_put())),

   {ok, Sock} = esio:socket("http://localhost:9200/bucket", []),
   ok = esio:add_(Sock, esio_FIXTURE:doc()),
   ok = esio:close(Sock),

   true = meck:validate(m_http).

%%
get_call(_Config) ->
   meck:expect(m_http, request, elastic_mock(esio_FIXTURE:elastic_ret_get()) ),
   Expect = esio_FIXTURE:doc(),

   {ok, Sock} = esio:socket("http://localhost:9200/bucket", []),
   {ok, Expect} = esio:get(Sock, esio_FIXTURE:key()),
   ok = esio:close(Sock),

   true = meck:validate(m_http).

%%
get_cast(_Config) ->
   meck:expect(m_http, request, elastic_mock(esio_FIXTURE:elastic_ret_get()) ),
   Expect = esio_FIXTURE:doc(),

   {ok, Sock} = esio:socket("http://localhost:9200/bucket", []),
   Ref = esio:get_(Sock, esio_FIXTURE:key()),
   receive {Ref, {ok, Expect}} -> ok end,
   ok = esio:close(Sock),

   true = meck:validate(m_http).

%%
get_send(_Config) ->
   meck:expect(m_http, request, elastic_mock(esio_FIXTURE:elastic_ret_get()) ),

   {ok, Sock} = esio:socket("http://localhost:9200/bucket", []),
   ok = esio:get_(Sock, esio_FIXTURE:key(), false),
   ok = esio:close(Sock),

   true = meck:validate(m_http).

%%
remove_call(_Config) ->
   meck:expect(m_http, request, elastic_mock(esio_FIXTURE:elastic_ret_rmv())),
   Expect = esio_FIXTURE:key(),

   {ok, Sock} = esio:socket("http://localhost:9200/bucket", []),
   {ok, Expect} = esio:remove(Sock, esio_FIXTURE:key()),
   ok = esio:close(Sock),

   true = meck:validate(m_http).

%%
remove_cast(_Config) ->
   meck:expect(m_http, request, elastic_mock(esio_FIXTURE:elastic_ret_rmv())),
   Expect = esio_FIXTURE:key(),

   {ok, Sock} = esio:socket("http://localhost:9200/bucket", []),
   Ref = esio:remove_(Sock, esio_FIXTURE:key(), true),
   receive {Ref, {ok, Expect}} -> ok end,
   ok = esio:close(Sock),

   true = meck:validate(m_http).

%%
remove_send(_Config) ->
   meck:expect(m_http, request, elastic_mock(esio_FIXTURE:elastic_ret_rmv())),

   {ok, Sock} = esio:socket("http://localhost:9200/bucket", []),
   ok = esio:remove_(Sock, esio_FIXTURE:key()),
   ok = esio:close(Sock),

   true = meck:validate(m_http).

%%
update_call(_Config) ->
   meck:expect(m_http, request, elastic_mock(esio_FIXTURE:elastic_ret_put())),
   Expect = esio_FIXTURE:key(),

   {ok, Sock} = esio:socket("http://localhost:9200/bucket", []),
   {ok, Expect} = esio:update(Sock, esio_FIXTURE:key(), esio_FIXTURE:doc()),
   ok = esio:close(Sock),

   true = meck:validate(m_http).

%%
update_cast(_Config) ->
   meck:expect(m_http, request, elastic_mock(esio_FIXTURE:elastic_ret_put())),
   Expect = esio_FIXTURE:key(),

   {ok, Sock} = esio:socket("http://localhost:9200/bucket", []),
   Ref = esio:update_(Sock, esio_FIXTURE:key(), esio_FIXTURE:doc(), true),
   receive {Ref, {ok, Expect}} -> ok end,
   ok = esio:close(Sock),

   true = meck:validate(m_http).

%%
update_send(_Config) ->
   meck:expect(m_http, request, elastic_mock(esio_FIXTURE:elastic_ret_put())),

   {ok, Sock} = esio:socket("http://localhost:9200/bucket", []),
   ok = esio:update_(Sock, esio_FIXTURE:key(), esio_FIXTURE:doc()),
   ok = esio:close(Sock),

   true = meck:validate(m_http).


%%
lookup_call(_Config) ->
   meck:expect(m_http, request, elastic_mock(esio_FIXTURE:elastic_ret_search())),
   Expect = esio_FIXTURE:elastic_ret_search(),

   {ok, Sock} = esio:socket("http://localhost:9200/bucket", []),
   {ok, Expect}  = esio:lookup(Sock, esio:pattern(#{a => 1})),
   ok = esio:close(Sock),

   true = meck:validate(m_http).

%%
lookup_cast(_Config) ->
   meck:expect(m_http, request, elastic_mock(esio_FIXTURE:elastic_ret_search())),
   Expect = esio_FIXTURE:elastic_ret_search(),

   {ok, Sock} = esio:socket("http://localhost:9200/bucket", []),
   Ref  = esio:lookup_(Sock, esio:pattern(#{a => 1})),
   receive {Ref, {ok, Expect}} -> ok end,
   ok = esio:close(Sock),

   true = meck:validate(m_http).

%%
lookup_send(_Config) ->
   meck:expect(m_http, request, elastic_mock(esio_FIXTURE:elastic_ret_search())),

   {ok, Sock} = esio:socket("http://localhost:9200/bucket", []),
   ok = esio:lookup_(Sock, esio:pattern(#{a => 1}), false),
   ok = esio:close(Sock),

   true = meck:validate(m_http).

%%
stream(_Config) ->
   meck:expect(m_http, request, elastic_stream(esio_FIXTURE:elastic_ret_search())),
   #{<<"hits">> := #{<<"hits">> := Expect}} = esio_FIXTURE:elastic_ret_search(),

   {ok, Sock} = esio:socket("http://localhost:9200/bucket", []),
   Expect = stream:list( esio:stream(Sock, esio:pattern(#{a => 1})) ),
   ok = esio:close(Sock),

   true = meck:validate(m_http).

%%
match(_Config) ->
   meck:expect(m_http, request, elastic_stream(esio_FIXTURE:elastic_ret_search())),
   #{<<"hits">> := #{<<"hits">> := Expect}} = esio_FIXTURE:elastic_ret_search(),

   {ok, Sock} = esio:socket("http://localhost:9200/bucket", []),
   Expect = stream:list( esio:match(Sock, #{a => 1}) ),
   ok = esio:close(Sock),

   true = meck:validate(m_http).

%%
bulk(_Config) ->
   meck:expect(m_http, request, elastic_mock(#{})),

   {ok, Sock} = esio:socket("http://localhost:9200/bucket", [bulk]),
   ok = esio:put(Sock, esio_FIXTURE:key(), esio_FIXTURE:doc()),

   ok = esio:close(Sock),

   true = meck:validate(m_http).

%%
bulk_sync(_Config) ->
   meck:expect(m_http, request, elastic_mock(#{})),

   {ok, Sock} = esio:socket("http://localhost:9200/bucket", [bulk, {n, 2}]),
   ok = esio:put(Sock, esio_FIXTURE:key(), esio_FIXTURE:doc()),
   ok = esio:put(Sock, esio_FIXTURE:key(), esio_FIXTURE:doc()),

   ok = esio:close(Sock),

   true = meck:validate(m_http).

%%
bulk_timeout(_Config) ->
   meck:expect(m_http, request, elastic_mock(#{})),

   {ok, Sock} = esio:socket("http://localhost:9200/bucket", [bulk, {t, 10}]),
   ok = esio:put(Sock, esio_FIXTURE:key(), esio_FIXTURE:doc()),
   ok = esio:put(Sock, esio_FIXTURE:key(), esio_FIXTURE:doc()),
   timer:sleep(20),
   ok = esio:put(Sock, esio_FIXTURE:key(), esio_FIXTURE:doc()),

   ok = esio:close(Sock),

   true = meck:validate(m_http).
   

%%%----------------------------------------------------------------------------   
%%%
%%% helper
%%%
%%%---------------------------------------------------------------------------- 

elastic_ping() ->
   fun(_) ->
      [identity ||
         esio_FIXTURE:elastic_nodes(),
         esio_FIXTURE:http(200, _),
         esio_FIXTURE:m_http(_)
      ]
   end.

elastic_mock(Response) ->
   fun(_) ->
      [identity ||
         cats:unit(#{
            <<"/_cat/nodes">> => esio_FIXTURE:http(200, esio_FIXTURE:elastic_nodes()), 
            default => esio_FIXTURE:http(200, Response)
         }),
         esio_FIXTURE:m_http(_)
      ]
   end.

elastic_stream(Response) ->
   fun(_) ->
      fun(#{req := [{_, Uri, _} | Json]} = State) -> 
         Http = case uri:path(Uri) of
            <<"/_cat/nodes">> ->
               esio_FIXTURE:http(200, esio_FIXTURE:elastic_nodes());
            _ ->
               case lens:get(lens:at(<<"from">>), jsx:decode(Json, [return_maps])) of
                  0 ->
                     esio_FIXTURE:http(200, Response);
                  _ ->
                     esio_FIXTURE:http(200, esio_FIXTURE:elastic_ret_search_empty())
               end
         end,
         [Http | State#{ret => Http}]
      end
   end.


