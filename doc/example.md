# `esio` usage examples

All examples are derived from original Elastic Search documentations. Examples expects that 
* Erlang application is started (use `esio:start().` command in Erlang console).
* Elastic Search cluster is running in docker container at localhost

## create index

See [http example here](https://www.elastic.co/guide/en/elasticsearch/reference/current/indices-create-index.html)

```erlang
{ok, Sock} = esio:socket("http://127.0.0.1:9200/twitter").
esio:schema(Sock, 
   #{
      settings => #{
         number_of_shards   => 3,
         number_of_replicas => 2
      }
   }
).
```

## define mappings

Note that Elastic has [announced removal of mapping types](https://www.elastic.co/guide/en/elasticsearch/reference/current/removal-of-types.html#_what_are_mapping_types). This library adapts to changes by removing mapping concept from its interface.

```elang
{ok, Sock} = esio:socket("http://127.0.0.1:9200/test").
esio:put(Sock, 
   #{
      settings => #{
         number_of_shards   => 1
      },
      mappings => #{
         _doc => #{
            properties => #{
               field1 => #{type => keyword}
            }
         }
      }
   }
).
```


## put value

See [http example here](https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-update.html)

```
{ok, Sock} = esio:socket("http://127.0.0.1:9200/test").
esio:put(Sock, "1", #{counter => 1, tags => [red]}).
```

## get value

```
{ok, Sock} = esio:socket("http://127.0.0.1:9200/test").
esio:get(Sock, "1").
```

## remove value

```
{ok, Sock} = esio:socket("http://127.0.0.1:9200/test").
esio:remove(Sock, "1").
```

