(ns c99.arithmetic
  (:use clojure.contrib.math)
  (:use clojure.contrib.profile)
  (:use clojure.test))

(require '(clojure.contrib [error-kit :as kit]))

(kit/deferror *disjoint-subsets-error* [] [n]
    {:msg (str "*** ОШИБКА :" n)
     :unhandled (kit/throw-msg NumberFormatException)})

;; For example, {1, 2, 3} and {4, 5, 6} are disjoint sets.
(defn c27-disjoint-subsets
  "Group the elements of a set into disjoint subsets."
  [set size]
  (cond
   (empty? size)
   (kit/raise *disjoint-subsets-error* "Данная размерность второго аргумента недостаточна для работы.: ")
   (empty? (rest size))
   (if (= (first size) (count set))
     (list set)
     (kit/raise *disjoint-subsets-error* "Cardinal mismatch |set| = ~A" ))
   :else ; «Множество есть совокупность различных элементов, мыслимая как единое целое» Бертран Рассел
   (loop [r nil s (clojure.core/set set) c size] ; 
     (if (empty? c)
       (reverse r)
       (recur
        (conj r (take (first c) s))     ; Conjoin: Adds value(s) to set
        (drop (first c) s)              ; As take, but removes the specified items and returns the rest.
        (rest c))))))

;; 9 рабочих разобьем на 3 непересекающиеся множества-подгруппы по 2, 3 и 4 человека
(c27-disjoint-subsets '(aldo beat carla david evi flip gary hugo ida) '(2 3 4))
;; ((aldo gary) (david carla ida) (beat hugo evi flip))

(count (combinations '(a b c d e f g h i j k l) 3)) ; 220

(deftest goldbach-conjecture-test
  (do
    (is (= '(5 37) (goldbach-conjecture-pair 42)))
    (is (= '(3 5) (goldbach-conjecture-pair 8)))
    (is (= '(3 17) (goldbach-conjecture-pair 20)))
    (is (= '(5 23) (goldbach-conjecture-pair 28)))))

(defn prime? [n]
    "Determines whether or not N is prime."
    (cond
     (not (number? n)) (throw (IllegalArgumentException. "prime? requires a number"))
     (< n 2)  false
     :else
     (empty? (filter #(= 0 %) (map #(rem n %) (range 2 n))))))


(defn goldbach-conjecture "Goldbach's conjecture. Write a predicate to find the two prime
    numbers that sum up to a given even integer." [n]
    (map #(sort (rest %))
	 (filter #(= (first %) true)
		 (for [x (range 0 (inc n)) y (range 0 n)]
		   (list (and (= (+ x y) n) (prime? x) (prime? y)) x y)))))

(defn goldbach-conjecture-pair [n]
  "Choose the pair where the difference (- b a) is maximized."
  (let [gc (goldbach-conjecture n)]
    (when-not (empty? gc)
     ;;(println "For" n "Goldbach's conjecture is wrong.")
     (next (first (sort-by first > (map #(let [f (first %) s (second %)]
					   (list (- s f) f s)) gc)))))))



(defn goldbach-list [lower upper & limit]
  "A list of Goldbach compositions."
  (count
   (let [limit (if (empty? limit) 0 (first limit))]
     (map #(let [gcp (goldbach-conjecture-pair %)]
             (when (and (> (count gcp) 1) (> (first gcp) limit))
               (println % "=" (first gcp) "+" (first (rest gcp)))))
          (filter even? (range lower (inc upper)))))))

;; http://jondotcomdotorg.net/tag/clojure/
;; http://www.google.com/webfonts/family?family=Inconsolata&subset=latin
;; http://rubykoans.com/

(comment

c99.arithmetic> (goldbach-list 2 3000 50)
992 = 73 + 919
1382 = 61 + 1321
1856 = 67 + 1789
1928 = 61 + 1867
2078 = 61 + 2017
2438 = 61 + 2377
2512 = 53 + 2459
2530 = 53 + 2477
2618 = 61 + 2557
2642 = 103 + 2539
2999
p99.arithmetic>

  )


;; http://unityenvironment.blogspot.com/
;; http://bigdingus.com/category/clojure/
;; http://code.google.com/p/jngmisc/source/browse/clojure/primes/primes.clj
;; http://githowto.com
