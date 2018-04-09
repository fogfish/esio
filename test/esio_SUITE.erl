%%
%%   Copyright (C) 2018 Dmitry Kolesnikov
%%
%%   This software may be modified and distributed under the terms
%%   of the MIT license.  See the LICENSE file for details.
%%
-module(esio_SUITE).

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
   put/1
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
         [socket, put]}
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

socket(_Config) ->
   meck:expect(m_http, request, fun elastic_ping/1),

   {ok, Sock} = esio:socket("http://localhost:9200/bucket", []),
   ok = esio:close(Sock),

   true = meck:validate(m_http).

put(_Config) ->
   meck:expect(m_http, request, elastic_mock(esio_FIXTURE:put())),

   {ok, Sock} = esio:socket("http://localhost:9200/bucket", []),
   {ok, <<"key">>} = esio:put(Sock, <<"key">>, #{text => <<"Hello World.">>}),
   ok = esio:close(Sock),

   true = meck:validate(m_http).



%%%----------------------------------------------------------------------------   
%%%
%%% helper
%%%
%%%---------------------------------------------------------------------------- 

elastic_ping(_) ->
   fun(State) -> 
      [esio_FIXTURE:ping() | State#{ret => esio_FIXTURE:ping()}] 
   end.

elastic_mock(Response) ->
   fun(_) ->
      fun(#{req := [{_, Uri, _} | _]} = State) -> 
         case uri:path(Uri) of
            <<"/_cat/nodes">> ->
               [esio_FIXTURE:ping() | State#{ret => esio_FIXTURE:ping()}];
            _ ->
               [Response | State#{ret => Response}] 
         end
      end
   end.

