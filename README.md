# zendoc

## Setup

### pre

1. java 8+
2. babashka
3. git

### clojure cli

1. create a document file

```
echo ':title "My first zd/note 🙂"' > docs/readme.zd
```

2. run zendoc engine

```
clojure -Sdeps '{:deps {zen-lang/zd {:git/url "git@github.com:zen-lang/zd.git" :sha "53ca914e73ff9cf6db75434d5d6dafe425e2b057"}}}' -M -m zd.dev docs zrc
```

Open http://localhost:8080

## tutorial

Semantic knowledge base

* every document is an edn map with qualified name
* documents are synced to a storage
* access database in a document
* annotate data for representation
* add schemas to validate data

Document is an ordered set of key-value pairs

keys start with ':'

```
:this-is-a-key "value"
```

Each key may have an annotation.
Annotation starts with a '^', following annotation name and optional edn value.

```
^table {:columns [:id]}
:items [{:id "p1"}, {:id "p2"}]

```

multi-line values are supported with `:key [content-type]/` notation

```
:content zentext/

Here is some zentext
* item 1

:another-key "..."


```
