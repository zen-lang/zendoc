(ns zd.datalog-test
  (:require
   [zd.datalog :as datalog]
   [clojure.test :refer [deftest is testing]]
   [zen.core :as zen]
   [xtdb.api :as xt]
   [matcho.core :as matcho]))

(def ztx (zen/new-context {}))

(defmacro match-parse [q pat]
  `(let [res# (datalog/parse-query ~q)]
     (matcho/match res# ~pat)
     res#))

(defmacro match-query [q pat]
  `(let [res# (datalog/datalog-query ztx  ~q)]
     (matcho/match res# ~pat)
     res#))

(defmacro match? [q pat]
  `(let [res# (datalog/datalog-sugar-query ztx  ~q)]
     (matcho/match res# ~pat)
     res#))

(defmacro match-find [cols fnd]
  `(let [res# (:find (datalog/make-find {:columns ~cols}))]
     (matcho/match res# ~fnd)
     res#))

(deftest datalog-engine

  (match-find  [{:name 'x}] ['x])
  (match-find  [{:name 'x}] ['x])
  (match-find '[{:name z, :prop :name, :label "z"}] ['(pull z [:name])])
  (match-find '[{:name z, :hidden true}] ['z])

  (match-parse "z :a 1\nc (count z)\n< desc c" {})
  (match-parse "z :a 1\n> z:name" {})

  (match-parse "x :zd/type c\n>c\n>x \n>(count x) | count" 
               '{:where [[x :zd/type c]],
                 :find [c x (count x)],})

  (matcho/match
      (match-parse "z :a 1\n>z:attr1\n>z:attr2" {})
    '{:columns [{:name z, :prop :attr1, :label "z"} {:name z, :prop :attr2, :label "z"}]})

  (match-find '[{:name z, :prop :attr1, :label "z"} {:name z, :prop :attr2, :label "z"}]
              '[(pull z [:attr1]) (pull z [:attr2])])

  (match-parse
   "x :attr y\n> x"
   '{:where [[x :attr y]] :find [x]})

  (match-parse
   "x :attr y\n> x:name"
   '{:where [[x :attr y]] :find [(pull x [:name])]})

  (match-parse
   "x :attr y\n> x:*"
   '{:where [[x :attr y]],
     :find [(pull x [*])]})

  (match-parse
   "x :attr y\n> x:?"
   '{:where [[x :attr y]],
     :find [(pull x [*])],
     :index {}})

  (match-parse
   "
x :attr y
y :other z
> x
< desc x
< limit 10
"
   '{:where [[x :attr y] [y :other z]],
     :order-by [[x :desc]],
     :limit 10,
     :find [x x]})

  (match-parse
   "x :attr #y"
   '{:where [[x :attr "'y"]],
     :find [x]})

  (match-parse
   "
x :attr y
> y
> (count x)
"
   '{:where [[x :attr y]],
     :find [y (count x)]})

  (match-parse
   "
x :attr y
c (count x)
> y | title
> c | count
"

   '{:where [[x :attr y] [c (count x)]],
     :find [y c]})

  (match-parse
   "
x :attr y
< desc x
"
   '{:where [[x :attr y]],
     :order-by [[x :desc]],
     :find [x]})

  (matcho/match
      (match-parse "x :zd/type c\nz (count x)\n> z" {})
    '{:where [[x :zd/type c] [z (count x)]],
      :find [z]})

  (match-parse
   "
x :attr y
z (count x)
> x
< desc z
"
   '{:where [[x :attr y] [z (count x)]],
     :order-by [[z :desc]],
     :find [x]})


  (datalog/datalog-put ztx {:zd/docname 'a :a 1})

  (match-query '{:where [[x :a y]],
                 :find [x y]
                 :order-by [[y :desc]],}
               [['a 1] nil?])


  (match? "x :a y\n> x\n> y"
          '{:result [[a 1]],
            :query {:where [[x :a y]],
                    :find [x y]},
            :columns ["x" "y"]})

  (datalog/datalog-put ztx {:zd/docname 'typed :zd/type 'zd.class})

  (match? "x :zd/type #zd.class\n> x"
          '{:result [[typed]],
            :columns ["x"]})

  (match? "x :zd/type c\n> x\n< desc c"
          '{:result [[typed]],
            :columns ["x"]})

  (match-parse "x :zd/type c\n> (count x) | cnt" {})

  (match? "x :zd/type c\n>c \n>(count x) | cnt"
          '{:result [[zd.class 1]],
            :query {:where [[x :zd/type c]], :find [c (count x)]},})

  (datalog/datalog-query ztx '{:where [[x :zd/type c]]
                               :find [(pull x [:zd/type]) (pull x [:b])]})



  )
