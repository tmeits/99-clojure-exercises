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
       (c99-or false true) true
       (c99-nor true true) false
       (c99-nor false false) true
       (c99-nor true false) false
       (c99-nor false true) false
       (c99-nand true true) false
       (c99-nand true false) true
       (c99-nand false true) true
       (c99-nand false false) true
       (c99-xor true true) false
       (c99-xor false false) false
       (c99-xor true false) true
       (c99-xor false true) true
       (c99-impl true true) true
       (c99-impl false false) true
       (c99-impl false true) true
       (c99-impl true false) false
       (c99-equ true true) true
       (c99-equ false false) true
       (c99-equ false true) false
       (c99-equ true false) false
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
(defn c99-nor [a b]
  "NOR is the negation of 'or'"
  (c99-not (c99-or a b)))
(defn c99-nand [a b]
  "NAND is the negation of 'and'"
  (c99-not (c99-and a b)))
(defn c99-xor [a b]
  "XOR is True if either a or b is True, but not if both are True"
  (cond
   (c99-and (= a true) (= b false)) true
   (c99-and (= a false) (= b true)) true
   :else false))
(defn c99-impl [a b]
  "IMPL is True if a implies b, equivalent to (not a) or (b)"
  (c99-or (c99-not a) b))
(defn c99-equ [a b]
  "EQU is True if a and b are equal"
  (= a b))

(comment
  http://www.haskell.org/haskellwiki/99_questions/Solutions/46
  http://www.clojure-toolbox.com/
  )

(run-tests)