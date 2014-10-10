(ns euler.common)

;; Natural Numbers
(def natural-numbers (iterate inc 1))

;; A corecursive fibonacci series generator
(def fib (lazy-cat [0 1] (map + fib (rest fib))))

;; A corecursive prime number generator
(def primes
  (lazy-cat [2 3]
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
