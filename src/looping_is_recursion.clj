(ns looping-is-recursion)

(defn power [base exp]
  (let [tail-helper (fn [carry n]
                       (if (zero? n)
                          carry
                          (recur (* carry base) (dec n))))]
    (tail-helper 1 exp)))

(defn last-element [a-seq]
  (let [tail-helper (fn [former latter]
                       (if (empty? latter)
                          former
                          (recur (first latter) (rest latter))))]
    (tail-helper (first a-seq) (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond (and (empty? seq1) (empty? seq2)) true
        (or (empty? seq1) (empty? seq2)) false
        (not (= (first seq1) (first seq2))) false
        :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [other-seq a-seq
         i 0]
        (cond (empty? other-seq) nil
              (pred (first other-seq)) i
              :else (recur (rest other-seq) (inc i)))))

(defn avg [a-seq]
  (loop [sum 0
         el-count 0
         other-seq a-seq]
        (if (empty? other-seq) 
          (/ sum el-count)
          (recur (+ sum (first other-seq)) (inc el-count) (rest other-seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [other-seq a-seq
         result-set #{}]
        (if (empty? other-seq)
          result-set
          (recur (rest other-seq) (toggle result-set (first other-seq))))))

(defn fast-fibo [n]
  (loop [prev-prev 0
         prev 1
         i 0]
    (if (= i n) 
      (+ prev-prev)
      (recur prev (+ prev-prev prev) (inc i)))))

(defn cut-at-repetition [a-seq]
  (loop [other-seq a-seq
         seen []]
    (if (or (empty? other-seq)
            (contains? (set seen) (first other-seq)))
      seen
      (recur (rest other-seq) (conj seen (first other-seq))))))
