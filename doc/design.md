# design considerations

## identity

Elastic Search defined the concept of _indexes_ and _type mappings_, see [example here](https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-index_.html). The index is a container for documents, the [mapping](https://www.elastic.co/guide/en/elasticsearch/reference/current/mapping.html) is logical group to accumulate related documents. For example, type-safe languages such as Scala might bind a cases class for particular mapping. 

The globally unique document reference is the triple: index id, mapping id and key. It naturally fit into [urn schema](https://en.wikipedia.org/wiki/Uniform_Resource_Name). The library uses urn data type `uri:urn()` as an object identity.   
```
urn:esio:index:mapping:key
```


to be continued
