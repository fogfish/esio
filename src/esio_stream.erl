%%
%%   Copyright (C) 2016 Zalando SE
%%
%%   This software may be modified and distributed under the terms
%%   of the MIT license.  See the LICENSE file for details.
%%
%% @doc
%%   stream is continues query, it is designed as functional stream with side-effect
%%   stream uses sort and search after features for continues stream sorting
-module(esio_stream). 
-include("esio.hrl").


-export([stream/2, match/2]).

%%
%% 
stream(Sock, #{sort := _} = Query) ->
   stream:takewhile(
      fun(X) -> X =/= eos end,
      stream:unfold(fun unfold/1, 
         #{
            state => [], 
            score =>  1,
            sock  => Sock, 
            q     => Query#{size => ?CONFIG_STREAM_CHUNK}
         }
      )
   );

stream(Sock, Query) ->
   stream(Sock, Query#{sort => ['_doc']}).

match(Sock, Pattern) ->
   stream(Sock, esio:pattern(Pattern)).


%%
%%
unfold(#{state := []} = Seed) ->
   case lookup(Seed) of
      {ok, #{<<"hits">> := #{<<"hits">> := []}}} ->
         {eos, Seed};

      {ok, #{<<"hits">> := #{<<"hits">> := Hits, <<"max_score">> := Score}}} ->
         unfold(
            Seed#{
               state => Hits,
               score => Score
            }
         )
   end;

unfold(#{state := [Hit | Hits], score := null} = Seed) ->
   {
      Hit, 
      search_after(Hit, Seed#{state := Hits})
   };

unfold(#{state := [#{<<"_score">> := Score} = Hit | Hits], score := Base} = Seed) ->
   {
      Hit#{<<"_score">> := maybe_div(Score, Base)}, 
      search_after(Hit, Seed#{state := Hits})
   }. 

%%
%%
lookup(#{sock := Sock, q := Query}) ->
   esio:lookup(Sock, Query).

%%
%%
maybe_div(Score, Base)
 when Base > 0 ->
   Score / Base;
maybe_div(Score, _) ->
   Score.

%%
%%
search_after(#{<<"sort">> := Sort}, #{q := Query} = Seed) ->
   Seed#{q => Query#{search_after => Sort}}.
