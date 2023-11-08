# zendoc

Semantic knowledge base

## Setup

### pre

#### mac
1. install java 8+
2. install git (for cloud editors)
* set merge strategy for the repo:
```
git config pull.rebase false
```
* set git email and username (for cloud editors)
```
git config user.name "editor name"
git config user.email "editor@zendoc.me"
```
3. clone this repo
4. get [brew](https://brew.sh/) package manager
5. install clojure with brew
```
brew install clojure/tools/clojure
```

### run with clojure cli

```
clojure -M:run
```

### run with clojure repl

```
clojure -M:nrepl
```
* start zd with (zd.core/start nil false)

### run as java jar

### getting started

Open http://localhost:4444
