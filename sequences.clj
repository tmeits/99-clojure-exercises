;; Ninety-Nine Problems In Clojure. Sequences.
(ns c99.seq)
(System/getProperty "java.class.path")

(defn clojure-seq-02 [coll]
  " ру Find the last but one box of a list."
  (let [r (reverse coll)]
    (reverse (cons (first r)                       
                   (list (first (next r)))))))
;; Test
(clojure-seq-02 '(a b c d))

;; Recursive solution
(defn clojure-seq-02-rec [coll]
   (cond 
   (empty? coll)  (print "Empty collection!")
   (empty? (rest coll)) (print "Collection too short!")
   (empty? (rest (rest coll))) coll
   :else   (clojure-seq-02-rec (rest coll))))

(clojure-seq-02-rec '(a b c d))

;; Recursive Clojure solution
(defn clojure-seq-02-rec2 [coll]
   (cond 
   (empty? coll)  (print "Empty collection!")
   (empty? (rest coll)) (print "Collection too short!")
   (empty? (rest (rest coll))) coll
   :else   (recur (rest coll))))

(clojure-seq-02-rec '(a b c d))
(clojure-seq-02-rec2 '(a b c d))

(doc clojure-seq-02)

;; http://clojuredocs.org/quickref/Clojure%20Core
;; http://pragmaticstudio.com/clojure/
;; http://riddell.us/ClojureSwankLeiningenWithEmacsOnLinux.html

;; https://gist.github.com/792198

;; http://blog.bensmann.com/setting-up-a-clojure-development-environment
;; http://p.hagelb.org/paredit-outline
;; http://hugoduncan.org/post/2010/swank_clojure_gets_a_break_with_the_local_environment.xhtml
;; http://code.google.com/p/fenoma/source/browse/trunk/prototype/Fonts/menlo/Menlo.ttc?r=2

(defn c03-element-at
  [coll n]
  ((vec coll) (- n 1))) 

(c03-element-at '(a b c d) 3)
3

(defn c03-element-at
  "Find the K'th element of a collections / sequences."
  [coll n]
  (cond
   (or (<= n 0) (> n (count coll))) nil
   :else
   ((vec coll) (- n 1))))

(c03-element-at '(a b c d) 4)
d

(defn c03-element-at-nth
  "Find the K'th element of a collections / sequences."
  [coll n]
  (cond
   (or (<= n 0) (> n (count coll))) nil
   :else
   (nth coll (- n 1))))

(c03-element-at-nth '(a b c d) 4)
d

(defn c03-element-at-rec
  "Find the K'th element of a collections / sequences.
   Recursive Clojure solution"
  [coll n]
  (cond
   (empty? coll) nil 
   (< n 0) (println "bad index < 0") 
   (> (- n 1) (count coll))  (println "bad index > length") 
   (= n 1) (first coll)
   :else
   (recur (rest coll) (- n 1))))
   ;(c03-element-at-rec (rest coll) (- n 1))))

(c03-element-at-rec '(a b c d) 1000)
d

;; http://mumble.net/~campbell/emacs/paredit.html

;; Find the number of elements of a collection.
(defn c04-length-collection [coll]
  "Returns the number of items in the collection. (count nil) returns
  0.  Also works on strings, arrays, and Java Collections and Maps"
  (count coll))

(c04-length-collection '(a b c d e f))
6

(defn c04-length-collection-recur [coll]
  "Find the number of elements of a collection."
  (if (empty? coll)
      0
      (+ 1 (c04-length-collection-recur (rest coll)))))

(c04-length-collection-recur '(a b c d))

(defn c04-length-collection-loop [coll]
  "Find the number of elements of a collection."
  (loop [c 0 coll coll]
    (if (empty? coll)
      c
      (recur (+ 1 c) (rest coll)))))

(c04-length-collection-loop '(a b c d e))

(defn lambda [v] 1)
(lambda 10)

(defn c04-length-collection-reduce [coll]
  "Find the number of elements of a collection."
  (reduce + (map #(if  (= % 0) 1 1) coll)))

(c04-length-collection-reduce [1 2 3 4 5])
5
(c04-length-collection-reduce '(1 2 3 4 5))
5

;; https://github.com/hugoduncan/swank-clj
;; https://groups.google.com/forum/#!topic/clojure/poOWf9iu6ec

;; translate
;; Обращение последовательности. Reverse a list.
(defn c05-reverse
  [coll] 
  "Return a new sequence containing the same elements but in reverse order."
  (when (reversible?  (vec coll))
    (reverse coll)))                       ;Стандартное решение

(c05-reverse '(a b c d e f))

(def rands (repeatedly rand))
(time (count (c05-reverse (take 1000000 rands)))) ; "Elapsed time: 936.137008 msecs"

(defn c05-reverse-loop
  "Обращение списка"
  [lst]
  (loop
      [new-lst '() old-lst lst]
    (if (next old-lst)                  ;пока в последовательности есть элементы
      (recur (cons (first old-lst) new-lst) (next old-lst))  ; собираем новую последовательность
      (cons (first old-lst) new-lst)))) ;последний штрих

(c05-reverse-loop '(a b c d e f))
(time (count (c05-reverse-loop (take 1000000 rands)))) ; "Elapsed time: 957.46096 msecs"



;; https://github.com/deobald/sicp-clojure.git
;; http://groups.google.com/group/clojure/browse_thread/thread/91d4f13090afb876/f3731f151757b71b?lnk=raot
;; http://technomancy.us/149 [ANN] Radically simplified Emacs and SLIME setup.
;; http://thinkrelevance.com/blog/2008/12/12/on-lisp-clojure.html
;; http://en.wikibooks.org/wiki/Clojure_Programming/Tutorials_and_Tips



;;  ПАЛИНДРОМЫ (перевертыши) - слова, читающиеся одинаково в обоих направлениях.

(defn c06-palindrome   
  "Функция определяющая является ли слово (последовательность) перевертышем."
  [sequence] 
  (= sequence (reverse sequence)))

(c06-palindrome '(R E D I V I D E R))
(c06-palindrome [1 2 3 4 3 2 1])

;; http://www.trouble.net.au/blog/korny/2011/06/03/10-clojure-one-liners/

;; Takes any nested combination of sequential things (lists, vectors,
;;  etc. ) and returns their contents as a single, flat sequence.
(defn c07-flatten  [sequence] 
  "Преобразовать многомерный список (список списков) в одномерный"
  (let [v (vec sequence)]
    (if (reversible? v)
      (flatten v)
      nil)))
(c07-flatten '(a (b (c d) e)))
(c07-flatten '[1 2 (3 [4] 5) [6 7]]) ; into (1 2 3 4 5 6 7)

;; Ленивая реализация от Rich Hickey 
(defn c07-flatten [x]
  (let [s? #(instance? clojure.lang.Sequential %)]
    (filter (complement s?) (tree-seq s? seq x))))

(flatten '[1 2 (3 [4 (5 "iva")] 5 "fred") [6 7]]) ; -> (1 2 3 4 5 "iva" 5 "fred" 6 7) 

  ;; https://github.com/stuarthalloway/practical-cl-clojure
  ;; https://github.com/stuarthalloway/programming-clojure
  ;; https://github.com/stuarthalloway/onlisp-clojure

;;  Eliminate consecutive duplicates of list elements.
;;  Вернем ленивую последовательность элементов с удаленными элементами дубликатами.
;;  Порядок элементов не менять.
(defn c08-compress [coll]
  (distinct (seq coll)))                ; distinct - duplicates removed

(c08-compress '(a a a a b c c a a d e e e e))

(defn find-first-dup [coll]
  "Поиск совпадений первого элемента в последовательности"
  (loop [f (first coll) i (next coll)]
    (cond
     (empty? i) i                       ;нет совпадений
     (= f (first i))  f ;Если совпадение с первым символом, возвращаем его
     :else
     (recur f (next i)))))              ;Сравним следующий символ
(find-first-dup '(a b cc cc d f))

(defn c08-compress-recur [coll]
  "Returns sequence of the elements of coll with duplicates removed"
  (loop [result (seq nil) s (seq coll)]
    (cond
     (empty? s) (reverse result)
     (=  (find-first-dup s) nil) (recur (cons (first s) result) (next s))
     :else
     (recur (cons (find-first-dup s) result)
            (filter #(not= % (find-first-dup s)) s)))))

(c08-compress-recur '(a a a a b c c a a d e e e e))    
(c08-compress-recur '(a b c d e))
(c08-compress-recur [1 2 3 4 5 1 2])
(c08-compress-recur nil)


(defn c08-test-compress []
  "Сто раз генерим последовательности случайной длины и сравниваем на корректность
   свою функцию со стандартной в Clojure."
  (loop [i 100  t true]
    (if (zero? i)
      t
      (let [r (map #(rand-int (+ % 1000)) (seq (range 100)))]
        (if (not= (c08-compress-recur r) (c08-compress r))
          false
          (recur (- i 1) t))))))
(c08-test-compress)

;; http://java.ociweb.com/mark/clojure/article.html

;; Собирает последующие дубликаты-копии элементов исходного списка в под списки
(defn c09-pack [coll] 
  "Pack consecutive duplicates of list elements into sublists.
   If a list contains repeated elements they should be placed in separate sublists."
  (defn pack-grouping [res elm]
    (if (= (first (first res)) elm)            
      (cons (cons elm (first res)) (rest res)) 
      (cons (list elm) res)))
  (reverse (reduce pack-grouping nil coll)))

;; Тест
(=  (c09-pack '(a a a a b c c a a d e e e e))
    '((a a a a) (b) (c c) (a a) (d) (e e e e))) ; -> true

;; Links
;; http://clojure.org/sequences
;; http://www.hackers-with-attitude.com/search/label/Clojure
;; https://github.com/mmcgrana/clj-stacktrace
;; https://github.com/kondratovich/web-clojure-demo
;; http://www.hackers-with-attitude.com/2009/08/intertactive-programming-with-clojure.html
;; http://zahardzhan.github.com/2010/clojure-setup-for-google-app-engine.html


(defn pack-grouping-test [res elm]
  (if (= (first (first res)) elm)            
    (cons (cons elm (first res)) (rest res)) 
    (do (println "res:" res " elm:" elm) (cons (list elm) res))))                  
(pack-grouping-test '(a b c) 'b)                  

c99>  (reduce #(list %1 %2) nil '(a a a a b c c a a d e e e e))
((((((((((((((nil a) a) a) a) b) c) c) a) a) d) e) e) e) e)
c99>
c99> (= (first (first '((a b c) d e))) 'a)
true
c99>

;;; L-99: Ninety-Nine Clojure Problems
;;  Последовательные дубликаты элементов списка записать в под списки вида (количество элемент)
(defn c10-encode [coll]
  "Consecutive duplicates of elements are encoded as lists (N E)"
  (reduce #(concat %1 (list (list (count %2) (first %2)))) nil (c09-pack coll)))

;; Test

(= (c10-encode
    '(a a a a b c c a a d e e e e))
   '((4 a) (1 b) (2 c) (2 a) (1 d) (4 e))) ; -> true

(defn c10-encode-map [coll]
  "Consecutive duplicates of elements are encoded as lists (N E)"
  (map #(list (count %) (first %)) (c09-pack coll)))

(c10-encode-map '(a a a a b c c a a d e e e e))


;; http://peepcode.com/products/functional-programming-with-clojure
;; https://github.com/mudphone/Quake-Log-Parser
;; http://codingkata.org/
;; http://www.slideshare.net/letronje/clojure-forbeginners


;; Только последовательные дубликаты элементов списка записать в под списки вида (количество элемент)
(defn c11-encode-modified [coll]
  "Only elements with duplicates are transferred as (N E) lists."
  (map #(if (= (count %) 1)
          (first %)
          (list (count %) (first %)))
       (c09-pack coll))) 

(c11-encode-modified '(a a a a b c c a a d e e e e))

;; Декодируем сроку после кодирования c11-encode-modified к начальному виду.
(defn c12-decode [coll]
  "Decode a run-length encoded list.
   Given a run-length code list generated as specified in problem l11-encode-modified. 
   Construct its uncompressed version."
  (flatten (map #(if (seq? %)
                   (repeat (first %) (next %))
                   %)
                (seq coll))))

(c12-decode (c11-encode-modified '(a a a a b c c a a d e e e e)))

(defn c13-encode-direct-solution1 [coll]
  "Run-length encoding of a list (direct solution)."
  (when (empty? coll) nil)
  (loop [result nil count 1 last-item (first coll) tmp-coll (next coll)]
    (if  (seq? tmp-coll)
      (do (println "Res:" result "Count:" count "last:" last-item "tmp:" tmp-coll)
          (if (= (first tmp-coll) last-item)
            (recur result (+ 1 count) last-item (next tmp-coll))
            (recur  (concat result (if (= 1 count ) (list last-item) (list count last-item)))
                    1 (first tmp-coll) (next tmp-coll))))
      (do (print "*** END ***") (concat result (list count last-item))))))

(c13-encode-direct-solution1 '(a a a a b c c a a d e e e e))

(defn c13-encode-direct-solution [coll]
  "Run-length encoding of a list (direct solution)."
  (when (empty? coll) nil)
  (loop [result nil count 1 last-item (first coll) tmp-coll (next coll)]
    (if  (seq? tmp-coll)
      (do (println "Res:" result "Count:" count "last:" last-item "tmp:" tmp-coll)
          (if (= (first tmp-coll) last-item)
            (recur result (+ 1 count) last-item (next tmp-coll))
            (recur (concat result (if (= 1 count ) (list last-item) (list count last-item)))
                    1 (first tmp-coll) (next tmp-coll))))
      (do (print "*** END ***") (concat result (list count last-item))))))

(c13-encode-direct-solution '(a a a a b c c a a d e e e e))
(ns )                    
;; http://neuralgorithm.org/documents/l-99-in-clojure/ 
;; http://m.3wa.com/2009/11/ninety-nine-problems-in-clojure-part-1-1-15/

(ns c99)
;; Seek and ye shall find. 
(use 'clojure.contrib.find-namespaces)
(use 'clojure.contrib.repl-utils)
(all-ns) ; Returns a list of all currently loaded namespaces.
;; C-99: Ninety-Nine Clojure Problems
(defn c14-duplicate-element   [coll]
  ^{:doc "Duplicate the elements of a list."}
  (flatten (map #(list % %) (seq coll))))

;; C-99: Ninety-Nine Clojure Problems
(defn c14-duplicate-element-reduce   [coll]
  ^{:doc "Duplicate the elements of a list."}
  (reverse (reduce #(conj (conj %1 %2) %2) nil (seq coll))))

(c14-duplicate-element-reduce '(a b c c d)) ; C-x C-e -> (a a b b c c c c d d)

 Example:
c99> (c14-duplicate-element '(a b c c d))
(a a b b c c c c d d)

;; http://www.learningclojure.com/2011/01/take-while-unstable.html
;; http://www.learningclojure.com/2010/10/latest-collection-of-filthy-hacks-for.html

;; M-x color-theme-montz

(defn c15-replicate [coll n]
  "Replicate the elements of a list a given number of times."
  (flatten                              ; разгладим подсписки
   (map                                 ; пройдемся по всем элементам коллекции
    #(clojure.core/repeat n %)          ; анонимная функция, где % переданный элемент
    ;; clojure.core/repeat возвращает ленивую последовательность или последовательность
    ;; где каждый элемент повторен н-ное количество раз
    coll)))

(c15-replicate '(a b c) 3) ; -> (a a a b b b c c c)

;; Drop every N'th element from a list
(defn c16-drop [coll n]
  "Удалить каждый N-ый элемент списка"
  (loop [i 1 r nil c coll]              ;рекурсивное решение
    (cond (empty? c) (clojure.core/reverse r)
          (not= i n) (recur (inc i) (conj r (first c)) (rest c))
          (= i n) (recur 1 r (rest c)))))

(c16-drop '(a b c d e f g h i k) 3) ; -> (a b d e g h k)

(defn c16-drop2 [coll n]
  "Удалить каждый N-ый элемент списка"
  (clojure.core/remove
   nil?                                 ;удаляем все элементы содержащие nil
   (map                                 ;идем по элементам двух последовательностей
    #(if (zero?                         ;возвращает истину если нуль и ложь иначе
          (rem %2 n))                   ;вычисляем остаток от деления lazy seq и если 0,
       nil %1)                          ;то элемент кратен удаляемому и подлежит удалению
    coll                                ;последовательность подлежащая прореживанию
    (iterate inc 1))))                  ;бесконечная последовательность '(1 2 3 ...)

(c16-drop2 '(a b c d e f g h i k) 3)
(comment
  c99> (type (c16-drop '(a b c d e f g h i k) 3))
  clojure.lang.PersistentList
  c99>) 


;; http://faustus.webatu.com/clj-quick-ref.html


;; Разделить список на два списка. Длина первого списка передается вторым параментром.
(defn c17-split  [coll n]
  "Split a list into two parts; the length of the first part is given."
  (seq [(clojure.core/take n coll) (clojure.core/drop n coll)])
  ;; (seq (split-at n coll)) Решение использующее входящую в clojure.core функцию
  ;; возвращающую вектор [(take n coll) (drop n coll)]
  )

(c17-split '(a b c d e f g h i k) 3); -> ((a b c) (d e f g h i k))

(ns ^{:doc "Ninety-Nine Clojure Problems."
      :author "IlynVA"}
  clojure.c99) 

(def color-theme  "M-x color-theme-billw")

;; Извлечение части списка. Нумерация с 1.
;; Первый параметр начала, второй конец извлеченного списка.
(defn c18-slice
  ^{:doc "Split a list into two parts; the length of the first part is given."}
  [coll start end]
  (cond 
   (= (type coll) java.lang.String) (clojure.core/subs coll start end) ;(strings? coll)
   (vector? coll) (clojure.core/subvec coll start end)
   (list? coll)  (seq (clojure.core/subvec (vec coll) start end))
   :ilse
   (do (println ":else")
       (take (- end start) (drop start coll)))))

(c18-slice  "abcdefghik" 3 7)             ;-> "defg"
(c18-slice  [1 2 3 4 5 6 7 8 9 10] 3 7)   ;-> [4 5 6 7]
(c18-slice  '(1 2 3 4 5 6 7 8 9 10) 3 7)  ;-> (4 5 6 7)
(c18-slice  {1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10} 3 7)  ;->([4 4] [5 5] [6 6] [7 7])
(c18-slice  '{1 a 2 b 3 c 4 d 5 e 6 f 7 g 8 h 9 i 10 k} 3 7) ;->([4 d] [5 e] [6 f] [7 g])
(c18-slice  '( a  b  c  d  e  f  g  h  i  k) 3 7) ;->(d e f g)

(defn c19-rotate
  ^{:doc "Повернуть список на N элементов влево."} 
  [coll num]
  (concat (clojure.core/subvec (vec coll) num (count coll))
          (clojure.core/subvec (vec coll) 0 num)))

(c19-rotate '(a b c d e f g h) 3) ;-> (d e f g h a b c)

(defn c19-rotate-split-at [coll n]
  (let [s (split-at n coll)]            ;vector of [(take n coll) (drop n coll)]
    (flatten (concat (rest s) (first s)))))

(c19-rotate-split-at '(a b c d e f g h) 3)

;; http://habrahabr.ru/blogs/Git/60347/

;; Решение использующее входящие в clojure.core функции
(defn c20-remote [s n]
  "Remove the K'th element from a list."
  (concat (butlast (take n s)) (drop n s)))

(c20-remote '(a b c d) 2)               ; -> (a c d)

;; Рекурсивное решение
(defn c20-remote-loop [s n]
  "Remove the K'th element from a list."
  (loop [i n res nil s s]
    (if (= 1 i)
      (concat (reverse res) (rest s))
      (recur (dec i) (conj res (first s)) (rest s)))))

(c20-remote-loop '(a b c d) 2)          ; -> (a c d)

; Вставить элемент в заданную позицию в список. Используем стандартные функции.
(defn c21-insert-at 
  ^{:doc "Insert an element at a given position into a list"}
  [insert-item coll index]
  (concat (take (dec index) coll) (list insert-item) (drop index coll)))

(c21-insert-at 'ilynva '(a b c d) 2)    ; -> (a ilynva c d)	

(defn c22-make-range-integers
  ^{:doc "Создать список содержащий все целые числа в заданном диапазоне."}
  [begin end]
  (list                                 ; несколько результатов
   (range begin (inc end))
   ;; Рекурсивное решение
   (loop [i begin res nil]
       (if (= i (inc end) )
         (reverse res)
         (recur (inc i) (conj res i))))))

(c22-make-range-integers 4 9)           ; -> ((4 5 6 7 8 9) (4 5 6 7 8 9))

; Создать список из списка. В новый список включить н-ое количество элементов,
; выбрав их случайным образом из начального списка.
(defn c23-random-select  
    ^{:doc "Extract a given number of randomly selected elements from a list. 
     The selected items shall be returned in a list."} 
    [coll n]
    ;; clojure.core/shuffle ([coll]) Return a random permutation of coll
    (seq (drop (- (count coll) n) (shuffle coll))))

(c23-random-select '(a b c d e f g h) 3)  ; -> (a e h)

(defn c23-random-select-loop
  "Рекурсивное решение"
  [coll n]
  (loop [i 0 res nil v (vec coll)]
    (if (= i n)
      res
      (recur (inc i) (conj res (v (rand-int (count coll)))) v))))

(c23-random-select-loop '(a b c d e f g h) 3)  ; -> (c a d)


;; Создать список из случайно выбранных чисел в количестве заданном первым аргументом. 
;; Числа сгенерить в диапазоне от 1 до заданного вторым аргументом 
(defn c24-lotto  [count range] 
  "Draw N different random numbers from the set 1..M."
  ;; Создать список из списка.
  (c23-random-select
  ;; Создать список содержащий все целые числа в заданном диапазоне
   (first (c22-make-range-integers 1 range)) count))

Test:
    > (c24-lotto 6 49)
    (49 8 11 21 39 6)

; Перетасовать список в случайном порядке
(defn c25-random-permutation [list] 
  "Generate a random permutation of the elements of a list."
  (c23-random-select list (count list)))
;; Hint: Use the solution of problem P23 or (clojure.core/shuffle list).
(c25-random-permutation '(a b c d e f)) ; -> (d c a e f b)

;; Sorting a list of lists according to length of sublists
clojure.c99> (clojure.core/sort #(< (count %1) (count %2))  '((a) (d e) (c f g h) (g)))
((a) (g) (d e) (c f g h))
clojure.c99> 

;; Отсортировать список по возрастанию длины сублистов.
(defn c28-sorting-length-sublists [coll]
  "Sorting a list of lists according to length of sublists"
  ;; Используем стандартную функцию.
  (sort  #(< (count %1) (count %2)) coll))

Test:
    > (c28-sorting-length-sublists '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
((o) (d e) (d e) (m n) (a b c) (f g h) (i j k l))


(defn c28-sorting-length-sublists-key [coll]
  "Sorting a list of lists according to length of sublists"
  (map #(first (rest %)) (sort-by #(first %) (map #(list (count %) %) coll))))

(c28-sorting-length-sublists-key
 '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o))) ; -> ((o) (d e) (d e) (m n) (a b c) (f g h) (i j k l))

(defn c28-sorting-length-sublists-set [coll]
  "Sorting a list of lists according to length of sublists"
  (reverse (set coll)))

(c28-sorting-length-sublists-set
 '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))  ; ->


(defn p26
  ^{:doc "Generate the combinations of K distinct objects chosen from the N elements of a list"}
  [n coll]
  (cond (empty? coll) []
	#(= 1 n) (map list coll)
	:else
	(concat (map #(cons (first coll) %) (p26 (dec n) (rest coll)))
		(p26 n (rest coll)))))

(p26 4 '(a b c d f g))

(ns ninety-nine-and-tests
  (:use clojure.test)
  (:use clojure.contrib.combinatorics)
  (:use clojure.contrib.error-kit))

(require '(clojure.contrib [error-kit :as kit]))

(kit/deferror *disjoint-subsets-error* [] [n]
    {:msg (str "*** ОШИБКА :" n)
     :unhandled (kit/throw-msg NumberFormatException)})
()
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
((aldo gary) (david carla ida) (beat hugo evi flip))
(comment
  Links:
  http://data-sorcery.org/2009/12/20/getting-started/
  http://groups.google.com/group/clojure/browse_thread/thread/2d1d91693887a45b?pli=1
  http://en.wikipedia.org/wiki/Disjoint_sets
  http://mathworld.wolfram.com/DisjointSets.html
  http://habrahabr.ru/blogs/algorithm/104772/
  http://richhickey.github.com/clojure-contrib/combinatorics-api.html
  https://github.com/plathrop/emacs-color-theme-solarized
  http://a1.twimg.com/profile_images/106480004/75df1763bf7d9adf75a73fafd0173982_bigger.png
)

ninety-nine-and-tests> (count (combinations '(a b c d e f g h i j k l) 3))
220
ninety-nine-and-tests> 

(with-test
  
  (defn factorial [n]
    (apply * (range 1 (inc n))))

  (defn cmn "Число сочетаний из n элементов по m"  [m n] 
    (/ (factorial n)
       (* (factorial (- n m)) (factorial m))))

  (defn create-set [n coll] "create set"
    (set (sort (clojure.core/take n(clojure.core/shuffle coll)))))
   
  (defn c26-generate-combination
    "Генерирование всех number-элементных подмножеств множества (1, ... ,n)."
    [n coll]
    (loop [i 1000000 res (set nil) count-combinations (cmn n (count coll)) coll coll]
      (if (or (= (count res) count-combinations) (= i 0))
        (do (println "Количество итераций= " i) res)
        (do ;(print "gen: " )
          (recur (dec i)
                 (conj res (create-set n coll))
                 count-combinations coll)))))
  
  (is (= (factorial 1)     1) "базовый случай")
  (is (= (factorial 4)     (* 1 2 3 4)) "first recursion")
  (is (= (factorial 10)    (apply * (range 1 11))) "general case" )
  (is (= (cmn 4 6)         15) "число возможных комбинаций")
  (is (= (cmn 4 6) (count (c26-generate-combination 4 '(1 2 3 4 5 6)))) "good generates")
  (is (= (cmn 3 5) (count (c26-generate-combination 3 '(мак роза тюльпан лилия гвоздика)))) "из 5 по 3"))
   

(run-tests 'ninety-nine-and-tests)

ninety-nine-and-tests> (run-tests 'ninety-nine-and-tests)

Testing ninety-nine-and-tests

Ran 1 tests containing 6 assertions.
0 failures, 0 errors.
{:type :summary, :test 1, :pass 6, :fail 0, :error 0}
ninety-nine-and-tests> 

(c26 '(a b c d) 2)


Links:
    сменить значение mailnews.tcptimeout с 60 до хотябы 300
    создать новый числовой ключ mail.pop3_response_timeout  со значением тоже хотябы 300
user_pref(“mail.pop3_response_timeout”, 300);

http://stackoverflow.com/questions/4690758/splitting-a-clojure-namespace-over-multiple-files
http://edu.mmcs.sfedu.ru/mod/assignment/view.php?id=1332
http://www.learningclojure.com/2010/11/yet-another-way-to-write-factorial.html
http://www.learningclojure.com/2009/11/namespaces-and-unit-tests.html
http://stackoverflow.com/questions/127704/algorithm-to-return-all-combinations-of-k-elements-from-n
http://www.informatimago.com/develop/lisp/l99/p26.lisp
http://jawher.net/2011/01/19/my-clojure-explained-solutions-to-the-s99-problems-21-to-26/

(defn permutations
  "Задание № 26.
Генерирование всех number-элементных подмножеств множества (1, ... ,n)."
  [n xs]
  (if (= 1 n)
    (map list xs)
    (reduce #(let [s (split-at (inc %2) xs)
                   ps (permutations (dec n) (last s))
                   a (last (first s))]
               (concat %1 (map (fn [p] (cons a p)) ps)))
            '() (range 0 (inc (- (count xs) n))))))


;; http://ethanschoonover.com/solarized

(use 'clojure.contrib.server-socket)


(create-server
  8088
  (fn [in out]
    (binding
      [*out* (java.io.PrintWriter. out)]
      (println "HTTP/1.0 200 OK")
      (println "Content-Type: text/html")
      (println "")
      (println "<h1>Мир ха-ха-ха, это мой первый веб сервер на Clojure!!</h1>")
      (flush))))

                                        


;; base1:     #93a1a1;
;; base3:     #fdf6e3;
;; http://www.color-hex.com/color/ просмотр цвета по коду.




