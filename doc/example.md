# `esio` usage examples

All examples are derived from original Elastic Search documentations. Examples expects that 
* Erlang application is started (use `esio:start().` command in Erlang console).
* Elastic Search cluster is running in docker container at 192.168.99.100

## create index

See [http example here](https://www.elastic.co/guide/en/elasticsearch/reference/current/indices-create-index.html)

```
{ok, Sock} = esio:socket("http://192.168.99.100:9200").
esio:put(Sock, "urn:es:twitter", 
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
{ok, Sock} = esio:socket("http://192.168.99.100:9200").
esio:put(Sock, "urn:es:test", 
   #{
      settings => #{
         number_of_shards   => 1
      },
      mappings => #{
         type1 => #{
            properties => #{
               field1 => #{type => string, index => not_analyzed}
            }
         }
      }
   }
).
```

## define mapping to existed index

```
{ok, Sock} = esio:socket("http://192.168.99.100:9200").
esio:put(Sock, "urn:es:test:_mappings:type2", 
   #{
      properties => #{
         field2 => #{type => string, index => not_analyzed}
      }
   }
).
```

## put value

See [http example here](https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-update.html)

```
{ok, Sock} = esio:socket("http://192.168.99.100:9200/test/type1").
esio:put(Sock, "urn:es:1", #{counter => 1, tags => [red]}).
```

## get value

```
{ok, Sock} = esio:socket("http://192.168.99.100:9200/test/type1").
esio:get(Sock, "urn:es:1").
```

## remove value

```
{ok, Sock} = esio:socket("http://192.168.99.100:9200/test/type1").
esio:remove(Sock, "urn:es:1").
```




