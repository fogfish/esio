%%
%%   Copyright (C) 2016 Zalando SE
%%
%%   This software may be modified and distributed under the terms
%%   of the MIT license.  See the LICENSE file for details.
%%
-module(esio_app).
-behaviour(application).
-author('dmitry.kolesnikov@zalando.fi').

-export([
   start/2
  ,stop/1
]).

%%
%%
start(_Type, _Args) ->
   esio_sup:start_link(). 

%%
%%
stop(_State) ->
   ok.
