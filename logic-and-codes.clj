(ns c99.log-cod
  (:use clojure.contrib.math)
  (:use clojure.contrib.profile)
  (:use clojure.test))

(defn and2 [a b]
  "AND is True if both a and b are True"
  (if a (if b true false) false))

(comment
  http://www.haskell.org/haskellwiki/99_questions/Solutions/46
  http://www.clojure-toolbox.com/
  )