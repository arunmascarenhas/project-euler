(ns euler.common)

;; Natural Numbers
(def natural-numbers (iterate inc 1M))

;; A corecursive fibonacci series generator
(def fib (lazy-cat [0M 1M] (map + fib (rest fib))))

;; A corecursive prime number generator
(def primes
  (lazy-cat [2M 3M]
            (filter
             (fn [x] (not-any?
                      (fn [y] (zero?
                               (rem x y)))
                      (take-while
                       (partial >= (Math/sqrt x))
                       primes)))
             (iterate (partial + 2)
                      (+ (last primes) 2)))))

;; A predicate function to test primality
(defn prime? [x]
  (not-any?
   (fn [y] (zero? (rem x y)))
   (range 2 (Math/sqrt x))))

;; A function to return the factors of a number
(defn factors-of [n]
  (into (sorted-set)
        (mapcat (fn [x] [x (/ n x)])
                (filter #(zero? (rem n %)) 
                        (range 1 (inc (Math/sqrt n)))))))

;; A function to return the prime factors of a number
(defn prime-factors [n] (filter prime? (factors-of n)))

;; A function to determine whether a number x is divisible by all the numbers nums
(defn divisible-by-all? [x nums]
  (not-any? (fn [y]
              (pos? (rem x y)))
            nums))

;; A function that returns the square of a number
(defn square-of [x]
  (* x x))

;; A function that returns the sum of a set of numbers
(defn sum-of [nums]
  (reduce + nums))

;; A dunction that squares the given numbers and returns the resulting sum
(defn sum-of-squares [nums]
  (sum-of (map square-of nums)))

;; A corecursive triangle number generator
(def triangle-numbers
  (lazy-cat [1 3]
            (map + (rest triangle-numbers)
                 (iterate inc 3))))

;; A corecursive factorial generator
(def factorial
  (lazy-cat [1M 2M]
            (map * (rest factorial)
                 (iterate inc 3M))))

;; A helper function to get the next term in the collatz sequence for a number
(defn- next-collatz-term [n]
  (if (even? n)
    (/ n 2)
    (inc (* 3 n))))

(def memoized-next-collatz-term (memoize next-collatz-term))

;; A function to generate the collatz sequence for a given number
(defn collatz-seq [n]
  (conj (vec 
         (take-while (partial not= 1) 
                     (iterate memoized-next-collatz-term n))) 
        1))




(defn euler-25 []
  (count (take-while #(> 1000 (count (seq (str %)))) 
                     common/fib)))
