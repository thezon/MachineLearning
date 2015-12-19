(ns projectESN.support
  (:require [clojure.core.matrix :as m]))

(def doLog false)

(defn get-file-datastr [file-name]  
    (read-string (slurp file-name)))

(defn create-input-samples [num-samples net-size]
  (into [] (repeatedly num-samples
                       (fn [] (into [] 
                                    (repeatedly net-size (fn [] (rand-nth [1 0]))))))))

(defn normalize-matrix [matrix]
  (into []
        (map #(clojure.core.matrix/normalise %) matrix)))

(defn rand-val [prob]
  "provides rand value between 0-1  with prob 'prob' otherwise 0"
  (if (< (rand) prob)
    (rand)
    0))

(defn prep-vec [size act-type-key]
  "create activation vector. act-type-key :rand :active :inactive"
  (into [] 
        (cond 
          (= act-type-key :rand)
          (repeatedly size #(rand-int 2))
          (= act-type-key :active)
          (repeat size 1)
          (= act-type-key :inactive)
          (repeat size 0)))) 

(defn trans-unit-sum [in-vec]
  (let [sum (apply + in-vec)]
  (map #(double (/ % sum)) in-vec)))

(defn loc-by-value [value vector]
  (into []
        (filter #(not (nil? %))
          (map #(if (= %1 value) %2) vector (range (count vector) )))))

(defn remove-nth [pos vec-in]
  (into []
        (concat
          (subvec vec-in 0 pos)
          (subvec vec-in (inc pos) (count vec-in)))))

(defn log-run[& entry]
  (if doLog
    (spit "ESNlog.csv" (str (apply str (interpose "," entry )) "\n") :append true)))