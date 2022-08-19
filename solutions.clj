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


