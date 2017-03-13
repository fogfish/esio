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
   put/3, put/4, put_/3, put_/4,
   add/2, add/3, add_/2, add_/3,
   get/2, get/3, get_/2, get_/3,
   remove/2, remove/3, remove_/2, remove_/3
]).
%% query and stream interface
-export([
   lookup/2, lookup/3, lookup/4, lookup_/2, lookup_/3, lookup_/4,
   stream/2, stream/3, 
   match/2,  match/3 
]).

%%
%% data types
-type url()  :: uri:uri().
-type key()  :: {urn, type(), uid()} | uid().
-type type() :: binary().
-type uid()  :: binary().
-type val()  :: map().
-type req()  :: map().
-type sock() :: pid().


%%
%% start application (RnD mode)
start() -> 
   applib:boot(?MODULE, []).

%%
%% create communication socket to Elastic Search.
%%  Options
%%   * uri:uri() - define the host and port for socket connection, the path
%%                 component defines index and type
%%   * bulk      - create bulk socket
%%   * n         - number of messages to buffer in bulk request 
%%   * t         - timeout to flush bulk request buffer
%%
-spec socket(url()) -> {ok, sock()} | {error, _}.
-spec socket(url(), [_]) -> {ok, sock()} | {error, _}.

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
   pipe:send(Sock, close).


%%
%% synchronous put operation
-spec put(sock(), key(), val()) -> {ok, _} | {error, _}.
-spec put(sock(), key(), val(), timeout()) -> {ok, _} | {error, _}.

put(Sock, Key, Val) ->
   put(Sock, Key, Val, ?TIMEOUT).

put(Sock, Key, Val, Timeout) ->
   req(Sock, {put, identity(Key), jsx:encode(Val)}, Timeout).


%%
%% asynchronous put operation
-spec put_(sock(), key(), val()) -> ok.
-spec put_(sock(), key(), val(), boolean()) -> ok | reference().

put_(Sock, Key, Val) ->
   put_(Sock, Key, Val, false).

put_(Sock, Key, Val, Flag) ->
   req_(Sock, {put, identity(Key), jsx:encode(Val)}, Flag).


%%
%%
-spec add(sock(), val()) -> {ok, _} | {error, _}.
-spec add(sock(), val(), timeout()) -> {ok, _} | {error, _}.

add(Sock, Val) ->
   add(Sock, Val, ?TIMEOUT).

add(Sock, Val, Timeout) ->
   req(Sock, {add, jsx:encode(Val)}, Timeout).


%%
%% asynchronous add operation
-spec add_(sock(), val()) -> ok.
-spec add_(sock(), val(), boolean()) -> ok | reference().

add_(Sock, Val) ->
   add_(Sock, Val, false).

add_(Sock, Val, Flag) ->
   req_(Sock, {add, jsx:encode(Val)}, Flag).


%%
%% synchronous get operation
-spec get(sock(), key()) -> {ok, val()} | {error, _}.
-spec get(sock(), key(), timeout()) -> {ok, val()} | {error, _}.

get(Sock, Key) ->
   get(Sock, Key, ?TIMEOUT).

get(Sock, Key, Timeout) ->
   req(Sock, {get, identity(Key)}, Timeout).


%%
%% asynchronous get operation
-spec get_(sock(), key()) -> {ok, val()} | {error, _}.
-spec get_(sock(), key(), boolean()) -> ok | reference().

get_(Sock, Key) ->
   get_(Sock, Key, true).

get_(Sock, Key, Flag) ->
   req_(Sock, {get, identity(Key)}, Flag).


%%
%% synchronous remove operation
-spec remove(sock(), key()) -> ok | {error, _}.
-spec remove(sock(), key(), timeout()) -> ok | {error, _}.

remove(Sock, Key) ->
   remove(Sock, Key, ?TIMEOUT).

remove(Sock, Key, Timeout) ->
   req(Sock, {remove, identity(Key)}, Timeout).


%%
%% asynchronous get operation
-spec remove_(sock(), key()) -> {ok, val()} | {error, _}.
-spec remove_(sock(), key(), boolean()) -> ok | reference().

remove_(Sock, Key) ->
   remove_(Sock, Key, false).

remove_(Sock, Key, Flag) ->
   req_(Sock, {remove, identity(Key)}, Flag).


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
   req(Sock, {lookup, identity(Uid), jsx:encode(Query)}, Timeout).

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
   req_(Sock, {lookup, identity(Uid), jsx:encode(Query)}, Flag).
   

%%
%% return data stream corresponding to query
-spec stream(sock(), req()) -> datum:stream(). 
-spec stream(sock(), key(), req()) -> datum:stream(). 

stream(Sock, Query) ->
   stream(Sock, ?WILDCARD, Query).

stream(Sock, Uid, Query)
 when is_map(Query) ->
   esio_stream:stream(Sock, identity(Uid), Query).

%%
%% pattern match data using elastic search boolean query
%%    https://www.elastic.co/guide/en/elasticsearch/guide/current/bool-query.html
-spec match(sock(), req()) -> datum:stream(). 
-spec match(sock(), key(), req()) -> datum:stream(). 

match(Sock, Pattern) ->
   match(Sock, ?WILDCARD, Pattern).

match(Sock, Uid, Pattern)
 when is_map(Pattern) ->
   esio_stream:match(Sock, identity(Uid), Pattern).


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

%%
%%
identity({urn, Type, Key}) ->
   {urn, scalar:s(Type), scalar:s(Key)};
identity(Key) ->
   {urn, undefined, scalar:s(Key)}.


