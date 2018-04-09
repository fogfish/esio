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

-export([stream/3, match/3]).

%%
%% 
stream(Sock, Uid, Query) ->
   stream:takewhile(
      fun(X) -> X =/= eos end,
      stream:unfold(fun unfold/1, 
         #{
            state => [], 
            score =>  1,
            sock  => Sock, 
            uid   => Uid, 
            q     => Query#{from => 0, size => ?CONFIG_STREAM_CHUNK}
         }
      )
   ).

match(Sock, Uid, Pattern) ->
   stream(Sock, Uid, pattern_to_query(Pattern)).

%%
%%
unfold(#{state := [], q := #{from := From} = Query} = Seed)
 when From < 10000 ->
   case lookup(Seed) of
      {ok, #{<<"hits">> := #{<<"hits">> := []}}} ->
         {eos, Seed};

      {ok, #{<<"hits">> := #{<<"hits">> := Hits, <<"max_score">> := Score}}} ->
         unfold(
            Seed#{
               state => Hits, 
               score => Score,
               q     => Query#{from => From + length(Hits)}
            }
         )
   end;   
unfold(#{state := [], q := #{from := From}} = Seed)
 when From >= 10000 ->
   {eos, Seed};

unfold(#{state := [H|T], score := null} = Seed) ->
   {H, Seed#{state := T}};
unfold(#{state := [#{<<"_score">> := Score} = H|T], score := Base} = Seed) ->
   {H#{<<"_score">> := maybe_div(Score, Base)}, Seed#{state := T}}. 

%%
%%
lookup(#{sock := Sock, uid := Uid, q := Query}) ->
   esio:lookup(Sock, Uid, Query).


%%
%%
pattern_to_query(Pattern) ->
   #{'query' => 
      #{bool => 
         #{must => [#{match => maps:put(Key, Val, #{})} || {Key, Val} <- maps:to_list(Pattern)] }
      }
   }.

%%
%%
maybe_div(Score, Base)
 when Base > 0 ->
   Score / Base;
maybe_div(Score, _) ->
   Score.
