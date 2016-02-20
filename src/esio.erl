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
   close/1
]).
%% key/value (hash-map like interface)
-export([
   put/3,
   put/4,
   put_/3,
   put_/4,
   get/2,
   get/3, 
   get_/2, 
   get_/3,
   remove/2, 
   remove/3, 
   remove_/2, 
   remove_/3
]).
%% query and stream interface
-export([
   lookup/2,
   lookup/3,
   lookup/4,
   lookup_/2,
   lookup_/3,
   lookup_/4,
   stream/2, 
   stream/3, 
   match/2, 
   match/3 
]).

%%
%% data types
-type key()  :: uri:urn().
-type val()  :: map().
-type req()  :: map().
-type sock() :: pid().


%%
%% start application (RnD mode)
start() -> 
   applib:boot(?MODULE, []).


%%
%% create communication socket to Elastic Search
%%
-spec socket(uri:uri()) -> {ok, sock()} | {error, _}.
-spec socket(uri:uri(), [_]) -> {ok, sock()} | {error, _}.

socket(Uri) ->
   socket(Uri, []).

socket(Uri, Opts) ->
   supervisor:start_child(esio_socket_sup, [uri:new(Uri), Opts]).

%%
%% close communication socket
%%
-spec close(sock()) -> ok.

close(Sock) ->
   pipe:send(Sock, close).

%%
%% synchronous put operation
-spec put(sock(), key(), val()) -> ok | {error, _}.
-spec put(sock(), key(), val(), timeout()) -> ok | {error, _}.

put(Sock, Key, Val) ->
   put(Sock, Key, Val, ?TIMEOUT).

put(Sock, Key, Val, Timeout)
 when is_map(Val) ->
   req(Sock, {put, uri:new(Key), jsx:encode(Val)}, Timeout).

%%
%% asynchronous put operation
-spec put_(sock(), key(), val()) -> ok.
-spec put_(sock(), key(), val(), boolean()) -> ok | reference().

put_(Sock, Key, Val) ->
   put_(Sock, Key, Val, false).

put_(Sock, Key, Val, Flag)
 when is_map(Val) ->
   req_(Sock, {put, uri:new(Key), jsx:encode(Val)}, Flag).


%%
%% synchronous get operation
-spec get(sock(), key()) -> {ok, val()} | {error, _}.
-spec get(sock(), key(), timeout()) -> {ok, val()} | {error, _}.

get(Sock, Key) ->
   get(Sock, Key, ?TIMEOUT).

get(Sock, Key, Timeout) ->
   req(Sock, {get, uri:new(Key)}, Timeout).


%%
%% asynchronous get operation
-spec get_(sock(), key()) -> {ok, val()} | {error, _}.
-spec get_(sock(), key(), boolean()) -> ok | reference().

get_(Sock, Key) ->
   get_(Sock, Key, true).

get_(Sock, Key, Flag) ->
   req_(Sock, {get, uri:new(Key)}, Flag).


%%
%% synchronous remove operation
-spec remove(sock(), key()) -> ok | {error, _}.
-spec remove(sock(), key(), timeout()) -> ok | {error, _}.

remove(Sock, Key) ->
   remove(Sock, Key, ?TIMEOUT).

remove(Sock, Key, Timeout) ->
   req(Sock, {remove, uri:new(Key)}, Timeout).


%%
%% asynchronous get operation
-spec remove_(sock(), key()) -> {ok, val()} | {error, _}.
-spec remove_(sock(), key(), boolean()) -> ok | reference().

remove_(Sock, Key) ->
   remove_(Sock, Key, false).

remove_(Sock, Key, Flag) ->
   req_(Sock, {remove, uri:new(Key)}, Flag).


%%
%% synchronous lookup (execute elastic search query)
-spec lookup(sock(), req()) -> {ok, val()} | {error, _}.
-spec lookup(sock(), key(), req()) -> {ok, val()} | {error, _}.
-spec lookup(sock(), key(), req(), timeout()) -> {ok, val()} | {error, _}.

lookup(Sock, Query) ->
   lookup(Sock, ?WILDCARD, Query).

lookup(Sock, Uid, Query) ->
   lookup(Sock, Uid, Query, ?TIMEOUT).

lookup(Sock, Uid, Query, Timeout) ->
   req(Sock, {lookup, uri:new(Uid), jsx:encode(Query)}, Timeout).

%%
%% asynchronous lookup
-spec lookup_(sock(), req()) -> ok | reference().
-spec lookup_(sock(), key(), req()) -> ok | reference().
-spec lookup_(sock(), key(), req(), boolean()) -> ok | reference().

lookup_(Sock, Query) ->
   lookup_(Sock, ?WILDCARD, Query).

lookup_(Sock, Uid, Query) ->
   lookup_(Sock, Uid, Query, true).

lookup_(Sock, Uid, Query, Flag) ->
   req_(Sock, {lookup, uri:new(Uid), jsx:encode(Query)}, Flag).
   

%%
%% return data stream corresponding to query
-spec stream(sock(), req()) -> datum:stream(). 
-spec stream(sock(), key(), req()) -> datum:stream(). 

stream(Sock, Query) ->
   stream(Sock, ?WILDCARD, Query).

stream(Sock, Uid, Query) ->
   esio_stream:stream(Sock, Uid, Query).

%%
%% pattern match data using elastic search boolean query
%%    https://www.elastic.co/guide/en/elasticsearch/guide/current/bool-query.html
-spec match(sock(), req()) -> datum:stream(). 
-spec match(sock(), key(), req()) -> datum:stream(). 

match(Sock, Pattern) ->
   match(Sock, ?WILDCARD, Pattern).

match(Sock, Uid, Pattern) ->
   esio_stream:match(Sock, Uid, Pattern).


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
   pipe:send(Sock, Req), ok.


