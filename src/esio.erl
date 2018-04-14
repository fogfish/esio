%%
%%   Copyright (C) 2016 Zalando SE
%%
%%   This software may be modified and distributed under the terms
%%   of the MIT license.  See the LICENSE file for details.
%%
-module(esio).
-author('dmitry.kolesnikov@zalando.fi').

-include("esio.hrl").

-export([start/0]).
%% socket interface
-export([
   socket/1,
   socket/2,
   close/1,
   schema/2
]).
%% key/value (hash-map like interface)
-export([
   put/3,
   put/4,
   put_/3,
   put_/4,
   add/2,
   add/3,
   add_/2,
   add_/3,
   get/2,
   get/3,
   get_/2,
   get_/3,
   remove/2,
   remove/3,
   remove_/2,
   remove_/3,
   update/3,
   update/4,
   update_/3,
   update_/4
]).
%% query and stream interface
-export([
   lookup/2,
   lookup/3,
   lookup_/2,
   lookup_/3,
   stream/2,
   match/2, 
   pattern/1
]).

%%
%% data types
-type url()  :: uri:uri().
-type key()  :: datum:option(_).
-type val()  :: #{}.
-type req()  :: #{}.
-type sock() :: pid().


%%
%% start application (RnD mode)
start() ->
   application:ensure_all_started(?MODULE).

%%
%% create communication socket to Elastic Search.
%%  Options
%%   * uri:uri() - define the host and port for socket connection, the path
%%                 component defines index and type
%%   * bulk      - create bulk socket
%%   * n         - number of messages to buffer in bulk request 
%%   * t         - timeout to flush bulk request buffer
%%
-spec socket(url()) -> datum:either( sock() ).
-spec socket(url(), [_]) -> datum:either( sock() ).

socket(Uri) ->
   socket(Uri, []).

socket(Uri, Opts) ->
   case opts:val(bulk, false, Opts) of
      false ->
         supervisor:start_child(esio_socket_sup, [uri:new(Uri), Opts]);
      true  ->
         supervisor:start_child(esio_socket_bulk_sup, [uri:new(Uri), Opts]);
      bulk  ->
         supervisor:start_child(esio_socket_bulk_sup, [uri:new(Uri), Opts])
   end.

%%
%% close communication socket
%%
-spec close(sock()) -> ok.

close(Sock) ->
   pipe:free(Sock).

%%
%% deploy index schema
-spec schema(sock(), val()) -> datum:either( key() ).
-spec schema(sock(), val(), timeout()) -> datum:either( key() ).

schema(Sock, Json) ->
   schema(Sock, Json, ?TIMEOUT).

schema(Sock, Json, Timeout) ->
   req(Sock, {schema, Json}, Timeout).


%%
%% synchronous put operation
-spec put(sock(), key(), val()) -> datum:either( key() ).
-spec put(sock(), key(), val(), timeout()) -> datum:either( key() ).

put(Sock, Key, Val) ->
   put(Sock, Key, Val, ?TIMEOUT).

put(Sock, Key, Val, Timeout) ->
   req(Sock, {put, scalar:s(Key), Val}, Timeout).


%%
%% asynchronous put operation
-spec put_(sock(), key(), val()) -> ok.
-spec put_(sock(), key(), val(), boolean()) -> ok | reference().

put_(Sock, Key, Val) ->
   put_(Sock, Key, Val, false).

put_(Sock, Key, Val, Flag) ->
   req_(Sock, {put, scalar:s(Key), Val}, Flag).


%%
%%
-spec add(sock(), val()) -> datum:either( key() ).
-spec add(sock(), val(), timeout()) -> datum:either( key() ).

add(Sock, Val) ->
   add(Sock, Val, ?TIMEOUT).

add(Sock, Val, Timeout) ->
   req(Sock, {add, Val}, Timeout).


%%
%% asynchronous add operation
-spec add_(sock(), val()) -> ok.
-spec add_(sock(), val(), boolean()) -> ok | reference().

add_(Sock, Val) ->
   add_(Sock, Val, false).

add_(Sock, Val, Flag) ->
   req_(Sock, {add, Val}, Flag).


%%
%% synchronous get operation
-spec get(sock(), key()) -> datum:either( val() ).
-spec get(sock(), key(), timeout()) -> datum:either( val() ).

get(Sock, Key) ->
   get(Sock, Key, ?TIMEOUT).

get(Sock, Key, Timeout) ->
   req(Sock, {get, scalar:s(Key)}, Timeout).


%%
%% asynchronous get operation
-spec get_(sock(), key()) -> ok.
-spec get_(sock(), key(), boolean()) -> ok | reference().

get_(Sock, Key) ->
   get_(Sock, Key, true).

get_(Sock, Key, Flag) ->
   req_(Sock, {get, scalar:s(Key)}, Flag).


%%
%% synchronous remove operation
-spec remove(sock(), key()) -> datum:either( key() ).
-spec remove(sock(), key(), timeout()) -> datum:either( key() ).

remove(Sock, Key) ->
   remove(Sock, Key, ?TIMEOUT).

remove(Sock, Key, Timeout) ->
   req(Sock, {remove, scalar:s(Key)}, Timeout).


%%
%% asynchronous get operation
-spec remove_(sock(), key()) -> ok.
-spec remove_(sock(), key(), boolean()) -> ok | reference().

remove_(Sock, Key) ->
   remove_(Sock, Key, false).

remove_(Sock, Key, Flag) ->
   req_(Sock, {remove, scalar:s(Key)}, Flag).

%%
%% synchronous put operation
-spec update(sock(), key(), val()) -> datum:either( key() ).
-spec update(sock(), key(), val(), timeout()) -> datum:either( key() ).

update(Sock, Key, Val) ->
   update(Sock, Key, Val, ?TIMEOUT).

update(Sock, Key, Val, Timeout) ->
   req(Sock, {update, scalar:s(Key), Val}, Timeout).


%%
%% asynchronous put operation
-spec update_(sock(), key(), val()) -> ok.
-spec update_(sock(), key(), val(), boolean()) -> ok | reference().

update_(Sock, Key, Val) ->
   update_(Sock, Key, Val, false).

update_(Sock, Key, Val, Flag) ->
   req_(Sock, {update, scalar:s(Key), Val}, Flag).

%%
%% synchronous lookup (execute elastic search query)
-spec lookup(sock(), req()) -> datum:either( val() ).
-spec lookup(sock(), req(), timeout()) -> datum:either( val() ).

lookup(Sock, Query) ->
   lookup(Sock, Query, ?TIMEOUT).

lookup(Sock, Query, Timeout) ->
   req(Sock, {lookup, Query}, Timeout).


%%
%% asynchronous lookup
-spec lookup_(sock(), req()) -> ok.
-spec lookup_(sock(), req(), boolean()) -> ok | reference().

lookup_(Sock, Query) ->
   lookup_(Sock, Query, true).

lookup_(Sock, Query, Flag) ->
   req_(Sock, {lookup, Query}, Flag).


%%
%% return data stream corresponding to query
-spec stream(sock(), req()) -> datum:stream(). 

stream(Sock, Query) ->
   esio_stream:stream(Sock, Query).

%%
%% pattern match data using elastic search boolean query
%%    https://www.elastic.co/guide/en/elasticsearch/guide/current/bool-query.html
-spec match(sock(), req()) -> datum:stream(). 

match(Sock, Pattern) ->
   esio_stream:match(Sock, Pattern).

%%
%%
-spec pattern(_) -> _.

pattern(Pattern) ->
   #{'query' => 
      #{bool => 
         #{must => [#{match => maps:put(Key, Val, #{})} || {Key, Val} <- maps:to_list(Pattern)] }
      }
   }.


%%-----------------------------------------------------------------------------
%%
%% private
%%
%%-----------------------------------------------------------------------------

%%
%% synchronous request
req(Sock, Req, Timeout) ->
   pipe:call(Sock, Req, Timeout).

%%
%% asynchronous request
req_(Sock, Req, true) ->
   pipe:cast(Sock, Req);

req_(Sock, Req, false) ->
   pipe:send(Sock, Req), 
   ok.
