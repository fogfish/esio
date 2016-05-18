#Design Considerations

###Identity

Elasticsearch has defined the concept of _indexes_ and _type mappings_; see [example](https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-index_.html). The index is a container for documents, and the [mapping](https://www.elastic.co/guide/en/elasticsearch/reference/current/mapping.html) is a logical grouping for accumulating related documents. Typesafe languages like Scala might bind a case class for particular mapping.

The globally unique document reference is the triple: index id, mapping id and key. It naturally fits into the [urn schema](https://en.wikipedia.org/wiki/Uniform_Resource_Name). Esio uses the urn data type `uri:urn()` as an object identity.   

>
> `<URN> ::= "urn:" <NID> ":" <NSS>`
>
> The leading urn: sequence is case-insensitive. <NID> is the namespace identifier, 
> which determines the syntactic interpretation of <NSS>, the namespace-specific string.
> 

Esio uses the following identity schema:
```
urn:es:cask:type:key
urn:es:type:key
urn:es:key
```
The library uses `<NID> = es`. The `<NSS>` consists of three segments: `cask`, `type` and `key`. If the urn defines an absolute identity, it uniquely identifies the Elasticsearch index (`cask`), document type (`type`) and document id (`key`). It is possible to define a relative `<NSS>` - skip `cask`, `type`.  

###Bulk Intake

Elasticsearch provides a [bulk API](https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-bulk.html). Esio implements a dedicated bulk socket that takes care of buffering and flushing the stream of data (use the `bulk` option to create a socket).
```
{ok, Sock} = esio:socket("http://192.168.99.100:9200", [bulk]).
```
