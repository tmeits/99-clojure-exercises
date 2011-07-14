(ns c99.log-cod
  (:use clojure.contrib.math)
  (:use clojure.contrib.profile)
  (:use clojure.test))

(deftest test-and-or-nor-nand-xor-impl-equ
  "Test codee for remaining logic functions"
  (do
    (is (= (not1 true) false))
    (is (= (not1 false) true))
    (is (= (and2 true true) true))
    (is (= (and2 true false) false))
    (is (= (and2 false true) false))
    (is (= (and2 false false) false))
    ))

(defn and2 [a b]
  "AND is True if both a and b are True"
  (if a (if b true false) false))
(defn not1 [a]
  "NOT negates a single Boolean argument"
  (if a false true))

(comment
  http://www.haskell.org/haskellwiki/99_questions/Solutions/46
  http://www.clojure-toolbox.com/
  )