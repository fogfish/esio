# Elastic Search I/O

**esio** is Erlang client for [Elasticsearch](https://www.elastic.co/products/elasticsearch), which offers a sophisticated [RESTful API](https://www.elastic.co/guide/en/elasticsearch/reference/current/docs.html). It implements a data access layer for Erlang applications and makes data and objects semantic-friendly with Erlang.

[![Changelog](https://img.shields.io/badge/changelog-latest-green.svg)](CHANGELOG.md) 
[![Build Status](https://secure.travis-ci.org/fogfish/esio.svg?branch=master)](http://travis-ci.org/fogfish/esio)
[![Coverage Status](https://coveralls.io/repos/github/fogfish/esio/badge.svg?branch=master)](https://coveralls.io/github/fogfish/esio?branch=master)

Albeit the "Zalando" mention, this repository is not affiliated with Zalando SE. **esio** was indeed born at Zalando, and Zalando AB holds copyright for parts of the code, but it is now being maintained outside the company, by its original authors and new contributors.


## Key features

Unlike other, similar projects, *esio* does not aim to provide a generic HTTP-proxy approach, but instead: 
* uses an **strict identity** schema to resolve naming crises and the hierarchical nature of Elastic Search indexes (index/_doc/key notation)
* offers **query templates** to counter the complexity of DSL 
* provides Erlang-friendly **pattern matching** for document lookup
* uses actor models for real-time **data streaming**
* provides interface to write **large bulks** of document to storage
* supports [**update api**](https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-update.html)
* integrates [in-memory cache](https://github.com/fogfish/cache)

See more details about the library at
* [design considerations](doc/design.md)
* [usage examples](doc/example.md) 

## Getting started

The latest version of the library is available from the `master` branch. All development, including new features and bug fixes, take place on the `master` branch using forking and pull requests as described in the contribution guidelines. 

The latest version of library is optimized for Elastic Search **6.0** or later.   


This library uses [rebar3](http://www.rebar3.org). Use the following code snippet to include *esio* in your `rebar.config`:
```
{esio, ".*",
   {git, "https://github.com/fogfish/esio", {branch, master}}
}
``` 

## Evaluate library  

Library exposes all its functionality through its [public interface](src/esio.erl). This enables client applications to manage socket connections to Elastic Search, execute basic key/value (hashmap-like) operations and search arbitrary documents in Elastic Search. 

You can experiment with libraries' features in your development console. This requires downloading Erlang/OTP version 18.0 or later and ElasticSearch. Docker container is easiest way to run a standalone instance of Elastic Search for development purposes.

```
docker run -it -p 9200:9200 docker.elastic.co/elasticsearch/elasticsearch:6.2.3
```

Build and run library in your development console:     
```
make && make run
```

Let's create an index and store some documents:

```erlang
%% 
%% start library
esio:start().

%%
%% open a socket to Elastic Search, socket is always bound with particular index 
{ok, Sock} = esio:socket("http://127.0.0.1:9200/my-index").

%%
%% create a new index with name `z`
esio:create_schema(Sock, #{settings => #{number_of_shards => 3, number_of_replicas => 1}}).

%%
%% append a new property to existed index
esio:schema(Sock, #{properties => #{x => #{type => keyword}}}).

%%
%% put documents to your index
esio:put(Sock, "1", #{title => <<"red color">>, tags => [red]}).
esio:put(Sock, "2", #{title => <<"yellow color">>, tags => [yellow]}).
esio:put(Sock, "3", #{title => <<"green color">>, tags => [green]}).
esio:put(Sock, "4", #{title => <<"black color">>, tags => [black]}).

%%
%% get document from index, identifiable by key `1`
esio:get(Sock, "1").

%%
%% match a documents to a pattern  
esio:match(Sock, #{tags => yellow}).

%%
%% close connection
esio:close(Sock).
```

See other examples [here](doc/example.md).


## How To Contribute

The library is MIT licensed and accepts contributions via GitHub pull requests:

* Fork the repository on GitHub
* Read build instructions
* Make a pull request

The build process requires [Erlang/OTP](http://www.erlang.org/downloads) version 18.0 or later and essential build tools.

**Build** and **run** service in your development console. The following command boots Erlang virtual machine and opens Erlang shell.

```bash
git clone https://github.com/fogfish/esio
cd esio
make
make run
```

### commit message

The commit message helps us to write a good release note, speed-up review process. The message should address two question what changed and why. The project follows the template defined by chapter [Contributing to a Project](http://git-scm.com/book/ch5-2.html) of Git book.

>
> Short (50 chars or less) summary of changes
>
> More detailed explanatory text, if necessary. Wrap it to about 72 characters or so. In some contexts, the first line is treated as the subject of an email and the rest of the text as the body. The blank line separating the summary from the body is critical (unless you omit the body entirely); tools like rebase can get confused if you run the two together.
> 
> Further paragraphs come after blank lines.
> 
> Bullet points are okay, too
> 
> Typically a hyphen or asterisk is used for the bullet, preceded by a single space, with blank lines in between, but conventions vary here
>
>

### bugs

If you experience any issues with the library, please let us know via [GitHub issues](https://github.com/fogfish/datum/issue). We appreciate detailed and accurate reports that help us to identity and replicate the issue. 

* **Specify** the configuration of your environment. Include which operating system you use and the versions of runtime environments. 

* **Attach** logs, screenshots and exceptions, in possible.

* **Reveal** the steps you took to reproduce the problem, include code snippet or links to your project.


### License

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
