(ns eulercl.core
  (:gen-class))

(use '[clojure.set]
     '[clojure.contrib.math :only (sqrt)]
     '[clojure.contrib.lazy-seqs :only (primes)]
     '[clojure.contrib.combinatorics :only (selections)])

(defn fib [a b]
  (cons a (lazy-seq (fib b (+ b a)))))

(defn factor? [of factor]
  (zero? (rem of factor)))

(defn prime-factors-of
  ([n] (prime-factors-of n primes))
  ([n restPrimes]
    (let [prime (first restPrimes)]
      (cond (> 2 n) (list)
            (factor? n prime) (conj (prime-factors-of (/ n prime)) prime)
            :else (prime-factors-of n (rest restPrimes))))))

(defn palindrome? [n]
  (let [s (str n)
        [left right] (split-at (/ (count s) 2) s)]
    (= left (reverse right))))

(defn palindromes-between [start end]
  (filter palindrome?
          (map #(apply * %) (selections (range start end) 2))))

(defn problem-1 []
  (reduce +
    (set (union
        (range 0 1000 3)
        (range 0 1000 5)))))

(defn problem-2 []
  (reduce +
    (take-while (partial >= 4E6)
      (filter even? (fib 1 1)))))

(defn problem-3
  ([] (problem-3 600851475143))
  ([n] (apply max (prime-factors-of n))))

(defn problem-4 []
  (apply max (palindromes-between 100 1000)))

(defn least-common-multiple [nums]
  4)

(defn numbers-divisible-by
  ; ([n] (numbers-divisible-by n n))
  ([n] (take 10000 (numbers-divisible-by n n)))
  ([n l] (cons l (lazy-seq (numbers-divisible-by n (+ l n))))))

(defn problem-5
  ([] (problem-5 (range 1 11)))
  ([r] (apply min (reduce intersection
                  (map set (map numbers-divisible-by r))))))

(defn -main
  [& args]
  (println (problem-4)))

