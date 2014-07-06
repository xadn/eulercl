(ns eulercl.core
  (:gen-class))

(use '[clojure.set]
     '[clojure.contrib.math :only (sqrt expt)]
     '[clojure.contrib.lazy-seqs :only (primes)]
     '[clojure.contrib.combinatorics :only (selections)])

(defrecord FactorPower [factor power])

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

(defn factor-powers-of [factors]
  (map (fn [x] (let [[f p] x] (FactorPower. f p)))
    (frequencies factors)))

(defn least-common-multiple [nums]
  (reduce *
    (map (comp (fn [x] (expt (:factor x) (:power x))) first #(sort-by :power > %))
         (vals (group-by :factor
               (flatten (map (comp factor-powers-of prime-factors-of) nums)))))))

(defn numbers-divisible-by
  ([n] (take 10000 (numbers-divisible-by n n)))
  ([n l] (cons l (lazy-seq (numbers-divisible-by n (+ l n))))))

(defn sum-of-squares [nums]
  (reduce + (map #(expt % 2) nums)))

(defn square-of-sums [nums]
  (expt (reduce + nums) 2))

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

(defn problem-5
  ([] (problem-5 (range 1 21)))
  ([r] (apply min (reduce intersection
                  (map set (map numbers-divisible-by r))))))

(defn problem-6
  ([] (problem-6 (range 1 101)))
  ([r] (- (square-of-sums r) (sum-of-squares r))))

(defn sanitize-block-of-numbers [text]
  (map (comp read-string str)
       (clojure.string/replace text #"\s" "")))

(defn slice-vec [my-vector len start]
  (subvec my-vector start (+ start len)))

(defn problem-7 [text numAdjacent]
  (let [nums (vec (sanitize-block-of-numbers text))
        endPos (- (count nums) numAdjacent)
        sliceStarts (range 0 (+ 1 endPos))]
    (apply max
           (map (comp (partial reduce *)
                      (partial slice-vec nums numAdjacent))
                sliceStarts))))

(defn -main
  [& args]
  (println (problem-4)))

