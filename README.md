# Elastic Search I/O

The library is HTTP client to [Elastic Search](https://www.elastic.co/products/elasticsearch) for Erlang application. 

The project is under development.

[![Build Status](https://secure.travis-ci.org/zalando/esio.svg?branch=master)](http://travis-ci.org/zalando/esio)

## inspiration

Elastic Search provides "sophisticated [RESTful API](https://www.elastic.co/guide/en/elasticsearch/reference/current/docs.html)". This library implements data access layer for Erlang applications and makes data and object semantic friendly to Erlang. Unlike other approach, this library do not aim to provide generic HTTP-proxy approach, it solves the following aspects 
* identity crisis through urn-base schema; it resolve hierarchical nature of index / type / key notation.
* complexity of DSL by offering query templates
* data streaming using actors models
* Erlang friendly pattern matching  



## introduction


### getting `esio`

The project is Erlang library, its latest version is available from master branch. All development, including new features and bug fixes, take place on master branch using forking and pull requests as described in [contribution guideline](doc/contribution.md). 

The library uses [rebar](https://github.com/rebar/rebar/wiki). Use the following code snippet to include the library to your `rebar.config`
```
   {esio, ".*",
      {git, "https://github.com/zalando/esio", {branch, master}}
   }
``` 

### running `esio`
to be defined


### continue to ...
* understand [the design considerations](doc/design.md)



## contributing
See [contribution guideline](doc/contribution.md) for details on PR submission.



## bugs
See [bug reporting](doc/bugs.md) for guidelines on raising issues. 



## contacts

* email: dmitry.kolesnikov@zalando.fi
* bugs: [here](https://github.com/zalando/esio/issues) 



# License

The MIT License (MIT)

Copyright (c) 2016 Zalando SE

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
