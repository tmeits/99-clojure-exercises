(comment
  "Solutions to Ninety-Nine Clojure Problems. Logic and codes.")
(ns c99.log-cod
  (:use clojure.contrib.math)
  (:use clojure.contrib.profile)
  (:use clojure.test))

(deftest logic-and-code "Test codee for remaining logic functions"
  (are [x y] (= x y)
       (c99-not true) false
       (c99-not false) true
       (c99-and true true) true
       (c99-and true false) false
       (c99-and false true) false
       (c99-and false false) false
       (c99-or true true) true
       (c99-or false false) false
       (c99-or true false) true
       (c99-or false true) true)
       ))

(defn c99-and [a b]
  "AND is True if both a and b are True"
  (if a (if b true false) false))
(defn c99-not [a]
  "NOT negates a single Boolean argument"
  (if a false true))
(defn c99-or [a b]
  "OR is True if a or b or both are True"
  (if a true (if b true false)))
(comment
  http://www.haskell.org/haskellwiki/99_questions/Solutions/46
  http://www.clojure-toolbox.com/
  )