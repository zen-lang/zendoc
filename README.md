# zendoc

## Setup

### pre

1. java 8+
2. clojure
3. babashka (optional)

### run with clojure cli

```
clojure -M:run
```

### run with babashka

```
bb run
```

### getting started

Open http://localhost:4444

check tutorial and customers-x knowledge base examples

## intro

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
