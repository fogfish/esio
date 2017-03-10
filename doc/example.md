# `esio` usage examples

All examples are derived from original Elastic Search documentations. Examples expects that 
* Erlang application is started (use `esio:start().` command in Erlang console).
* Elastic Search cluster is running in docker container at localhost

## create index

See [http example here](https://www.elastic.co/guide/en/elasticsearch/reference/current/indices-create-index.html)

```
{ok, Schema} = esio:socket("http://127.0.0.1:9200").
esio:put(Schema, "twitter", 
   #{
      settings => #{
         number_of_shards   => 3,
         number_of_replicas => 2
      }
   }
).
```

## define mappings

```
{ok, Schema} = esio:socket("http://127.0.0.1:9200").
esio:put(Schema, "test", 
   #{
      settings => #{
         number_of_shards   => 1
      },
      mappings => #{
         type1 => #{
            properties => #{
               field1 => #{type => keyword}
            }
         }
      }
   }
).
```

## define mapping to existed index

```
{ok, Schema} = esio:socket("http://127.0.0.1:9200/test").
esio:put(Schema, {urn, "_mappings", "type2"}, 
   #{
      properties => #{
         field2 => #{type => keyword}
      }
   }
).
```

## put value

See [http example here](https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-update.html)

```
{ok, Sock} = esio:socket("http://127.0.0.1:9200/test/type1").
esio:put(Sock, "1", #{counter => 1, tags => [red]}).
```

## get value

```
{ok, Sock} = esio:socket("http://127.0.0.1:9200/test/type1").
esio:get(Sock, "1").
```

## remove value

```
{ok, Sock} = esio:socket("http://127.0.0.1:9200/test/type1").
esio:remove(Sock, "1").
```




