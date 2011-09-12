(comment
  "Solutions to Ninety-Nine Clojure Problems. Logic and codes.")

;; ../99-clojure-exercises$ lein repl
;; user=> (load-file "logic-and-codes.clj")

(ns c99.log-cod
  (:use clojure.contrib.math)
  (:require clojure.contrib.string)
  (:use clojure.contrib.pprint)
  (:use clojure.contrib.profile)
  (:use clojure.test))

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

(defn c99-truth-tables [a b sexp]
  "Truth tables for logical expressions.
  Define a function that takes a logical expression (a function of two variables)
  and prints the truth table."
  (for [x '(true false) y '(true false)]
    ((fn [p1 p2] (def A p1) (def B p2)  (list p1 p2 (eval sexp))) x y)))

(defn c99-truth-tables-two [a b sexp]
  "Truth tables for logical expressions.
  Define a function that takes a logical expression (a function of two variables)
  and prints the truth table."
  (cond
   (= a b) '()
   :else 
   (for [i '(true false) j '(true false)]
     ((fn [p1 p2] (intern *ns* (symbol (str a)) p1) (intern *ns* (symbol (str b)) p2)
        (list p1 p2 (eval sexp))) i j))))

;; http://stackoverflow.com/questions/4208680/in-clojure-how-can-i-undef-a-var-from-a-namespace

(defn c99-truth-tables-unmap [a b sexp]   "Truth tables for logical expressions."
  (let
      [res
       (for [i '(true false) j '(true false)]
         ((fn [p1 p2]
            (intern *ns* (symbol (str a)) p1) (intern *ns* (symbol (str b)) p2)
            (list p1 p2 (if (eval sexp) "trues"  "false"))) i j))]
    (pr-str res)
    (ns-unmap *ns* a) (ns-unmap *ns* b)  res))

(defn- t-f [n] (if (= (rem n 2) 0) true false))
(defn- create-table [n]  (take n (repeat  (map t-f (range n)))))

(defn c99-truth-tables-doseq [a b sexp]  "Truth tables for logical expressions."
  (doseq [i (map t-f (range 2)) j (map t-f (range 2))]
    (intern *ns* (symbol (str a)) i) (intern *ns* (symbol (str b)) j)
    (println i j (eval sexp)))
  (ns-unmap *ns* a) (ns-unmap *ns* b))
 
(defn  replace-char [ch1 ch2  str]
  (clojure.contrib.string/map-str #(if (= % ch1) ch2 (identity %)) str))

(defn  truth-tables [s-expr & abc] "Truth tables for logical expressions."
  (for [i '(true false) j '(true false)]
    (list i j 
          (load-string (replace-char (first (pr-str (first (next abc)))) j
                        (replace-char (first (pr-str (first abc))) i
                                      (pr-str s-expr)))))))
(pprint (truth-tables  '(and A (or A B)) 'A 'B))
;; http://taop.rpod.ru/172639.html
;; http://whollyweirdwyrd.blogspot.com/2010/01/recursive-walk-on-string.html

(deftest table-test "Truth tables for logical expressions."
  (is (= (c99-truth-tables 'A 'B '(and A (or A B)))
         '((true true true) (true false true) (false true false) (false false false))))
  (is (= (c99-truth-tables-two 'D 'D '(and D (or D F))) '()))
  (is (= (c99-truth-tables-two 'A 'B '(and A (or A B)))
         '((true true true) (true false true) (false true false) (false false false))))
  (is (= (c99-truth-tables-unmap 'A 'B '(and A (or A B)))
         '((true true true) (true false true) (false true false) (false false false))))
  (is (= (truth-tables  '(and A (or A B)) 'A 'B)
         '((true true true) (true false true) (false true false) (false false false)))))

(deftest logic-and-code "Test code for remaining logic functions"
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
       (c99-equ true false) false))

;; Links:
;; http://www.haskell.org/haskellwiki/99_questions/Solutions/46
;; http://www.clojure-toolbox.com/
;; http://habrahabr.ru/blogs/Git/117187/#habracut
;; http://stackoverflow.com/questions/678867/how-to-defn-a-function-from-string-in-clojure
;; http://stackoverflow.com/questions/3407921/clojure-resolving-function-from-string-name
;; http://stackoverflow.com/questions/1523240/let-vs-binding-in-clojure
;; http://www.exampler.com/blog/2010/09/01/editing-trees-in-clojure-with-clojurezip/
;; Tips
;; C-\
;; http://clojure.org/repl_and_main
;; http://www.mari.ru/mmlab/home/lisp/LECTION3/index.html
;; http://blog.gaz-jones.com/post/2528825514/command-line-applications-in-clojure

;; http://progit.org/book/ru/
;; http://hyperpolyglot.org/lisp


;; http://www.rsdn.ru/article/alg/gray.xml
;; http://clojure-euler.wikispaces.com/Problem+001

(defn bin-to-gray [b]
  "Binary to Gray code conversion"
  (bit-xor (bit-shift-right b 1) b))

(defn print-gray [b] ""
  (defn make-string [n s] ""
    (reduce #(str %1 %2) (repeat n s)))
  (let [c (read-string (str "2r" (make-string b "1")))]
    (map #(if (< (count %) b) (str (make-string (- b (count %)) "0") %) %)
         (map #(Integer/toString (bin-to-gray %) 2) (range 0 (+ 1 c))))))

(deftest test-bin-to-gray
  (is (= (Integer/toString (bin-to-gray 7) 2) "100"))
  (is (= (Integer/toString (bin-to-gray 15) 2) "1000"))
  (is (= (nth (print-gray 4) 14) "1001")))

;; http://en.wikipedia.org/wiki/Gray_code
;; http://www.informatimago.com/develop/lisp/l99/p49.lisp
 
(defn gray [n]
  "Код Грея для n бит может быть рекурсивно построен на основе кода для n–1 бит путём переворачивания списка бит (то есть записыванием кодов в обратном порядке), конкатенации исходного и перевёрнутого списков, дописывания нулей в начало каждого кода в исходном списке и единиц — в начало кодов в перевёрнутом списке. Так, для генерации списка для n = 3 бит на основании кодов для двух бит необходимо выполнить следующие шаги:
Коды для n = 2 бит: 	00, 01, 11, 10 	
Перевёрнутый список кодов: 		10, 11, 01, 00
Объединённый список: 	00, 01, 11, 10 	10, 11, 01, 00
К начальному списку дописаны нули: 	000, 001, 011, 010 	10, 11, 01, 00
К перевёрнутому списку дописаны единицы: 	000, 001, 011, 010 	110, 111, 101, 100"
  (if (= 1 n)
    (do (println "0,1") (list "0" "1"))
    (let [gray-one (gray (- n 1))] 
      (concat (map #(str  "0" %)
                gray-one)
           (map #(str  "1" %)
                (reverse gray-one))))))

; P49 (**) Gray code.
(comment "An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,")
(comment "n = 1: C(1) = ['0','1']. ")
(comment "n = 2: C(2) = ['00','01','11','10'].")
(comment "n = 3: C(3) = ['000','001','011','010','110','111','101','100'].")
 
(defn gray2 [n]
  (defn list-to-string [l] (reduce #(str %1 %2) l))
  (loop [nleft n combos '(())]
    (if (zero? nleft)
      (map #(list-to-string %) combos)  ; convert list-lists to list-string
      (recur (dec nleft) (concat (map #(conj % 1) combos) (map #(conj % 0) combos))))))

;; http://www.gettingclojure.com/cookbook:numbers
;; Bitwise operations: bit-and bit-or bit-xor bit-not bit-shift-right bit-shift-left

(run-tests)

