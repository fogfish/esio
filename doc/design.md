# design considerations

## identity

Elastic Search defined the concept of _indexes_ and _type mappings_, see [example here](https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-index_.html). The index is a container for documents, the [mapping](https://www.elastic.co/guide/en/elasticsearch/reference/current/mapping.html) is logical group to accumulate related documents. For example, type-safe languages such as Scala might bind a cases class for particular mapping.

The globally unique document reference is the triple: index id, mapping id and key. It naturally fit into [urn schema](https://en.wikipedia.org/wiki/Uniform_Resource_Name). The library uses urn data type `uri:urn()` as an object identity.   

>
> `<URN> ::= "urn:" <NID> ":" <NSS>`
>
> The leading urn: sequence is case-insensitive. <NID> is the namespace identifier, 
> which determines the syntactic interpretation of <NSS>, the namespace-specific string.
> 

The library uses the following identity schema:
```
urn:es:cask:type:key
urn:es:type:key
urn:es:key
```
The library uses `<NID> = es`. The `<NSS>` consist of three segments `cask`, `type` and `key` if urn defines a absolute identity; it uniquely identifies Elastic Search index (`cask`), document type (`type`) and document id (`key`). It is possible to define relative `<NSS>` - skip `cask`, `type`.  



## bulk intake

Elastic Search provides [bulk api](https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-bulk.html). The library implement a dedicated bulk socket that takes care of buffering and flushing stream of data (use `bulk` option to create socket).
```
{ok, Sock} = esio:socket("http://192.168.99.100:9200", [bulk]).
```


to be continued
