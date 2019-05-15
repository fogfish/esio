-module(esio_FIXTURE).

-export([
   http/2,
   m_http/1,
   elastic_nodes/0,
   elastic_ret_put/0,
   elastic_ret_schema/0,
   elastic_ret_get/0,
   elastic_ret_rmv/0,
   elastic_ret_search/0,
   elastic_ret_search_empty/0,

   bucket/0,
   key/0,
   doc/0
]).

%%
%% helper to mock m_http:request/1
http(Code, Payload) ->
   [{Code, undefined, []}, Payload].

m_http(Http)
 when is_list(Http) ->
   fun(State) -> 
      [Http | State#{ret => Http}]
   end;

m_http(Http)
 when is_map(Http) ->
   fun(#{req := [{_, Uri, _} | _]} = State) ->
      Path = path(Uri),
      case Http of
         #{Path := Route} -> 
            [Route | State#{ret => Route}];
         #{default := Route} ->
            [Route | State#{ret => Route}]
      end
   end.

path({uri, _, _} = Uri) -> uri:path(Uri);
path(Uri) -> path(uri:new(Uri)).

%%
%%
elastic_nodes() ->
   <<"172.17.0.2 16 96 0 0.10 0.04 0.01 mdi * ndNIMi-\n">>.

%%
%%
elastic_ret_schema() -> 
   #{
      <<"acknowledged">> => true,
      <<"index">> => bucket(),
      <<"shards_acknowledged">> => true
   }.

%%
%%
elastic_ret_put() ->
   #{
      <<"_id">>    => key(),
      <<"_index">> => bucket(),
      <<"_primary_term">> => 1,
      <<"_seq_no">> => 30,
      <<"_shards">> => #{<<"failed">> => 0,<<"successful">> => 1,<<"total">> => 2},
      <<"_type">> => <<"_doc">>,
      <<"_version">> => 31,
      <<"result">> => <<"updated">>
   }.   

%%
%%
elastic_ret_get() ->
   #{
      <<"_id">> => key(),
      <<"_index">> => bucket(),
      <<"_source">> => doc(),
      <<"_type">> => <<"_doc">>,
      <<"_version">> => 31,
      <<"found">> => true
   }.

%%
%%
elastic_ret_rmv() ->
   #{
      <<"_id">> => key(),
      <<"_index">> => bucket(),
      <<"_primary_term">> => 1,
      <<"_seq_no">> => 31,
      <<"_shards">> => #{<<"failed">> => 0,<<"successful">> => 1, <<"total">> => 2},
      <<"_type">> => <<"_doc">>,
      <<"_version">> => 32,
      <<"result">> => <<"deleted">>
   }.   

%%
%%
elastic_ret_search() ->
   #{
      <<"_shards">> => #{<<"failed">> => 0,<<"successful">> => 1, <<"total">> => 2},
      <<"hits">> => #{
         <<"hits">> => [
            #{
               <<"_id">> => <<"key1">>,
               <<"_index">> => <<"bucket">>,
               <<"_score">> => 1.0,
               <<"_source">> => doc(),
               <<"_type">> => <<"_doc">>,
               <<"sort">> => [1]
            },
            #{
               <<"_id">> => <<"key2">>,
               <<"_index">> => <<"bucket">>,
               <<"_score">> => 1.0,
               <<"_source">> => doc(),
               <<"_type">> => <<"_doc">>,
               <<"sort">> => [2]
            }
         ],
         <<"max_score">> => 1.0,
         <<"total">> => 4
      },
      <<"timed_out">> => false,
      <<"took">> => 18
   }.

%%
%%
elastic_ret_search_empty() ->
   #{
      <<"_shards">> => #{<<"failed">> => 0,<<"successful">> => 1, <<"total">> => 2},
      <<"hits">> => #{
         <<"total">> => 0,
         <<"max_score">> => 0,
         <<"hits">> => []
      },
      <<"timed_out">> => false,
      <<"took">> => 18
   }.

%%
%%
bucket() ->
   <<"bucket">>.

%%
%%
key() ->
   <<"key">>.

%%
%%
doc() ->
   #{<<"text">> => <<"Hello World.">>}.

