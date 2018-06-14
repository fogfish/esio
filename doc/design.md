# Design Considerations

### Identity

Note that Elastic has [announced removal of mapping types](https://www.elastic.co/guide/en/elasticsearch/reference/current/removal-of-types.html#_what_are_mapping_types). This library adapts to changes by removing mapping concept from its interface.

The library uses a strong binding schema for sockets. The path component of socket `uri()` defines default value for index, the key unique identify the document. The library internally constructs requests to valid HTTP endpoint using this information


```erlang
%%
%% socket is not bound to index, therefore any I/O operation should fail. The destination index unknown.
esio:socket("http://127.0.0.1:9200").

%%
%% socket is bound to index `cask`. The default type mappings _doc is used.
%% it only supports keys in binary serializable format that explicit identify document
esio:socket("http://127.0.0.1:9200/cask").

%%
%% socket is bound to index `cask`, `type` mappings is ignored. 
esio:socket("http://127.0.0.1:9200/cask/type").
```

### Bulk Intake

Elastic Search provides a [bulk API](https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-bulk.html). *esio* implements a dedicated bulk socket that takes care of buffering and flushing the stream of data (use the `bulk` option to create a socket).
```
{ok, Sock} = esio:socket("http://127.0.0.1:9200/cask", [bulk]).
```
