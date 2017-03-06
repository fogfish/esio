%%
%%   Copyright (C) 2016 Zalando SE
%%
%%   This software may be modified and distributed under the terms
%%   of the MIT license.  See the LICENSE file for details.
%%
-module(esio_identity).

-include("esio.hrl").

%%
%% The globally unique document reference is the triple: index id, mapping id and key.
%%
%% The socket uri might define
%%  * nothing (e.g. http://localhost:9200)
%%  * index   (e.g. http://localhost:9200/index)
%%  * index and mapping (e.g. http://localhost:9200/index/type)
%%
%% The key identity can define either index id, mapping id and key.
%% Key specific definitions are ignored if socket is bound to index and type 
-export([
   new/1,
   search/2,
   keyval/2
]).


%%
%%
new(Uri) ->
   uri:new(Uri).


%%
%%
search(Uri, Key) ->
   uri:segments(
      search_identity(
         segments(Uri),
         Key
      ),
      Uri
   ).

search_identity(undefined, ?WILDCARD) ->
   [<<"_search">>];
search_identity([?WILDCARD], _) ->
   [<<"_search">>];
search_identity(undefined, {Cask, Type}) ->
   [scalar:s(Cask), scalar:s(Type), <<"_search">>];
search_identity([Cask], {_, Type}) ->
   [scalar:s(Cask), scalar:s(Type), <<"_search">>];
search_identity([Cask], ?WILDCARD) ->
   [scalar:s(Cask), <<"_search">>];
search_identity([Cask], Type) ->
   [scalar:s(Cask), scalar:s(Type), <<"_search">>];
search_identity([Cask, Type], _) ->
   [scalar:s(Cask), scalar:s(Type), <<"_search">>].


%%
%%
keyval(Uri, Key) ->
   uri:segments(
      keyval_identity(
         segments(Uri),
         Key
      ),
      Uri
   ).

keyval_identity(undefined, undefined) ->
   [];
keyval_identity(undefined, {Cask, Type, Key}) ->
   [scalar:s(Cask), scalar:s(Type), scalar:s(Key)];
keyval_identity(Default, undefined) ->
   Default;

keyval_identity([Cask], {_, Type, Key}) ->
   [scalar:s(Cask), scalar:s(Type), scalar:s(Key)];
keyval_identity([Cask], {Type, Key}) ->
   [scalar:s(Cask), scalar:s(Type), scalar:s(Key)];

keyval_identity([Cask, Type], {_, _, Key}) ->
   [scalar:s(Cask), scalar:s(Type), scalar:s(Key)];
keyval_identity([Cask, Type], {_, Key}) ->
   [scalar:s(Cask), scalar:s(Type), scalar:s(Key)];
keyval_identity([Cask, Type], Key) ->
   [scalar:s(Cask), scalar:s(Type), scalar:s(Key)].

%%
%%
segments(Uri) ->
   case uri:segments(Uri) of
      [] -> undefined;
      X  -> X
   end.
