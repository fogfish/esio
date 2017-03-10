#Design Considerations

###Identity


Elasticsearch has defined the concept of _indexes_ and _type mappings_; see [example](https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-index_.html). The index is a container for documents, and the [mapping](https://www.elastic.co/guide/en/elasticsearch/reference/current/mapping.html) is a logical grouping for accumulating related documents. Typesafe languages like Scala might bind a case class for particular mapping.

The globally unique document reference is the triple: index id, mapping id and key. The library uses a strong binding schema for sockets. The path component of socket `uri()` defines default value for index and type mapping. 

```
%%
%% socket is not bound to index, therefore keys are absolute path (always)
{ok, Schema} = esio:socket("http://127.0.0.1:9200").

%%
%% socket is bound to index `test` (the bindings of type mappings are not defined)
%% it only supports keys in `urn()` format
{ok, Socket} = esio:socket("http://127.0.0.1:9200/cask").

%%
%% socket is bound to index `test` and` type` mappings, it supports keys as scalar value or `urn()` tuple 
{ok, Socket} = esio:socket("http://127.0.0.1:9200/cask/type").
```

The library uses urn tuples `{urn, type(), key()}` to explicitly define destination `type` mappings for i/o operation. 


###Bulk Intake

Elasticsearch provides a [bulk API](https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-bulk.html). Esio implements a dedicated bulk socket that takes care of buffering and flushing the stream of data (use the `bulk` option to create a socket).
```
{ok, Sock} = esio:socket("http://192.168.99.100:9200/cask", [bulk]).
```
