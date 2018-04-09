-module(esio_FIXTURE).

-export([
   ping/0,
   put/0
]).


ping() ->
   [
      {200, <<"OK">>, []}, 
      <<"172.17.0.2 16 96 0 0.10 0.04 0.01 mdi * ndNIMi-\n">>
   ].


put() ->
   [
      {200, <<"OK">>, []},
      #{
         <<"_id">>    => <<"key">>,
         <<"_index">> => <<"bucket">>,
         <<"_primary_term">> => 1,
         <<"_seq_no">> => 30,
         <<"_shards">> => #{<<"failed">> => 0,<<"successful">> => 1,<<"total">> => 2},
         <<"_type">> => <<"_doc">>,
         <<"_version">> => 31,
         <<"result">> => <<"updated">>
      }
   ].
