%%
%%   Copyright (C) 2016 Zalando SE
%%
%%   This software may be modified and distributed under the terms
%%   of the MIT license.  See the LICENSE file for details.
%%
%% @doc
%%   stream is continues query, it is designed as functional stream with side-effect
-module(esio_stream). 
-include("esio.hrl").

-export([new/3]).

%%
%% 
new(Sock, Uid, Query) ->
   stream:takewhile(
      fun(X) -> X =/= eos end,
      stream:unfold(fun unfold/1, 
         #{
            state => [], 
            sock  => Sock, 
            uid   => Uid, 
            q     => Query#{from => 0, size => ?CONFIG_STREAM_CHUNK}
         }
      )
   ).
   
%%
%%
unfold(#{state := [], q := #{from := From} = Query} = Seed) ->
   case lookup(Seed) of
      {ok, #{<<"hits">> := []}} ->
         {eos, Seed};

      {ok, #{<<"hits">> := Hits}} ->
         unfold(
            Seed#{
               state => Hits, 
               q     => Query#{from => From + length(Hits)}
            }
         )
   end;   

unfold(#{state := [H|T]} = Seed) ->
   {H, Seed#{state := T}}. 

%%
%%
lookup(#{sock := Sock, uid := Uid, q := Query}) ->
   esio:lookup(Sock, Uid, Query).

