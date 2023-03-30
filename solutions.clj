;; https://4clojure.oxal.org/

;; Problem 1

true


;; Problem 2

4


;; Problem 3

"HELLO WORLD"


;; Problem 4

:a :b :c


;; Problem 5

'(1 2 3 4)


;; Problem 6

:a :b :c


;; Problem 7

[1 2 3 4]


;; Problem 8

(set '(:a :b :c :d))


;; Problem 9

2


;; Problem 10

20


;; Problem 11

[:b 2]


;; Problem 12

3


;; Problem 13

[20 30 40]


;; Problem 14

8


;; Problem 15

#(* 2 %)


;; Problem 16

(fn [name] (+ "Hello, " name "!"))


;; Problem 17

'(6 7 8)


;; Problem 18

'(6 7)


;; Problem 19

(defn last-el [l] 
  (if (empty? (rest l)) 
    (first l) 
    (last-el (rest l))))


;; Problem 20

(defn sl [l] 
  (if (= 2 (count l)) 
    (first l) 
    (sl (rest l))))


;; Problem 21

(defn n [l idx]
  (if (= idx 0)
    (first l)
    (n (rest l) (- idx 1))))


;; Problem 22

(defn len [seq]
  (if (empty? seq)
    0
    (+ 1 (len (rest seq)))))


;; Problem 23

(defn rev [s]
  (if (empty? s)
    ()
    (concat (rev (rest s)) (list (first s)))))


;; Problem 24

(defn sum [l]
  (if (empty? l)
    0
    (+ (first l) (sum (rest l)))))


;; Problem 25

(defn odds [l]
  (if (empty? l)
    ()
    (if (= (mod (first l) 2) 1)
      (conj (odds (rest l)) (first l))
      (odds (rest l)))))


;; Problem 26

defn fib [n]
  ((defn fibn [n fst snd]
    (if (> n 0)
      (conj (fibn (- n 1) snd (+ fst snd)) fst)
     '()))
   n
   1
   1))


;; Problem 27

(defn palindrome [s] (= (reverse (reverse s)) (reverse s)))


;; Problem 28

(defn flat [s]
  (letfn [
    (flatten-one [s]
      (if (coll? s)
        (flatten-seq s)
        (list s)))
     (flatten-seq [s]
       (if (empty? s)
         s
         (concat (flatten-one (first s)) (flatten-seq (rest s)))))]
  (flatten-seq s)))


;; Problem 29

;; The following solutions work in a REPL but not in the online evaluator

;; My best guess is the REPL cannot load java classes as it runs in a browser

(#(filter (fn is-uc [chr] (Character/isUpperCase chr)) %) "aAbB")

;; However, the reason this solution doesn't work remains a mystery to me
;; It works in the REPL and I would assume the clojure standard library
;; is independent of the JVM. Javascript users probably need to join strings
;; as well

;; clojure.string/join works in the browser

(defn filter-uc [s] 
  (clojure.string/join 
   "" 
   ((fn as-seq [chars] 
     (filter 
       (fn is-uc [chr] 
         (or 
           (= chr \A) (= chr \B) (= chr \C) (= chr \D) (= chr \E)
           (= chr \F) (= chr \G) (= chr \H) (= chr \I) (= chr \J)
           (= chr \K) (= chr \L) (= chr \M) (= chr \N) (= chr \O)
           (= chr \P) (= chr \U) (= chr \R) (= chr \S) (= chr \T)
           (= chr \U) (= chr \V) (= chr \W) (= chr \X) (= chr \Y)
           (= chr \Z))) 
       chars)) 
    s)))

;; Same as above

(fn filter-uc [s]
  (letfn [
       (is-uc [chr] 
         (or 
           (= chr \A) (= chr \B) (= chr \C) (= chr \D) (= chr \E)
           (= chr \F) (= chr \G) (= chr \H) (= chr \I) (= chr \J)
           (= chr \K) (= chr \L) (= chr \M) (= chr \N) (= chr \O)
           (= chr \P) (= chr \U) (= chr \R) (= chr \S) (= chr \T)
           (= chr \U) (= chr \V) (= chr \W) (= chr \X) (= chr \Y)
           (= chr \Z)))
       
       (filter-chars [s]
         (cond
           (empty? s) ""
           (is-uc (first s)) (str (first s) (filter-chars (rest s)))
           :else (filter-chars (rest s))))] 

       (filter-chars s))) 

;; The web apps handling of strings is weird
;; Perhaps a disparity between the JS and JVM implementations of Clojure
; (rest "asdf")
; user=> ("s" "d" "f")

;; Problem 30

; rcd -> remove consecutire duplicates
(defn rcd [sequence]
  (cond
    (empty? sequence)
      sequence
    (empty? (rest sequence))
      sequence
    (= (first sequence) (first (rest sequence)))
      (rcd (rest sequence))
    :else
      (cons (first sequence) (rcd (rest sequence)))))

;; Problem 31


(defn pack [sequence]
  (cond
    (empty? sequence)
      sequence
    (empty? (rest sequence))
      (list sequence)
    (= (first sequence) (first (rest sequence)))
      (let
        [res (pack (rest sequence))]
        (cons
          (cons (first sequence) (first res))
          (rest res)))
    :else
      (cons (list (first sequence)) (pack (rest sequence)))))
