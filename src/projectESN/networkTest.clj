(ns projectESN.networkTest
  (:require [incanter.core :as incant]
            [incanter.charts :as chart]
            [incanter.stats :as stat]))


(defn t-test [vec-one vec-two]
  (incanter.stats/t-test vec-one :y vec-two))


(defn exper-counts [experiment-mat]
  "counts all active neurons for each obeservation of a sample"
  (map 
    (fn [sample] (map #(apply + % ) sample)) experiment-mat))

(defn exper-counts-bucket [experiment-mat bucket-size]
  "exper counts in by bucket size"
  (map (fn[x] (partition bucket-size x))
       (map 
         (fn [sample] (map #(apply + % ) sample)) experiment-mat)))

(defn compare-exper-counts [experiment-mat control-mat size]
  "t test between to experiments returns p-value"
  (let [exp-buckets (exper-counts-bucket experiment-mat  size)
        count-buckets (exper-counts-bucket control-mat size)]
    (map (fn [ex cn] 
           (map #(:p-value (projectESN.networkTest/t-test %1 %2)) ex cn))
         exp-buckets count-buckets)))


(defn get-rand-compare [experiment control num-samples num-buckets bucket-size]
  "number-samples: number of samples in exper  num-buckets: number of buckets sequenctial in time bucket-size: how many time units per bucket"
  (map #(nth (nth (compare-exper-counts experiment control bucket-size) (rand-int num-samples)) %) (range num-buckets)))

(comment  "delete me"
(defn randomized-sample-test[] 
(map #(vector (:p-value %) (:x-mean %))
     (get-rand-compare experiment control 10 5 5))))

(defn exper-diff [experiment-mat]
  "diff all active neurons for each obeservation of a sample"
  (map
    (fn [in] (flatten 
               (map (fn [v x] 
                    [(apply + (apply map #(java.lang.Math/abs (- %1 %2)) v))
                      (apply + (apply map #(java.lang.Math/abs (- %1 %2)) x))])
                  (partition 2 (butlast in))
                    (partition 2 (rest in))))) experiment-mat))

(defn diff-counts [bucket-size exper-mat]
  "Gets diff counts of buckets from experement matrix"
  (reduce 
    #(map 
       (fn [x y] 
         (float (/ (+ x y) (count %1)))) %1 %2)
    (map 
      (fn [v]
        (map
          (fn [x] (apply + x))
          (partition bucket-size v))) (exper-diff exper-mat))))

(defn auto-reg-diff-ttest [exp-matrix sample-num bucket-size]
  "performs t test on a sample of some size agains the next contigous (time unit) sample"
  (interleave
    (map  #(vector (:p-value %) (:x-mean %))
         (map (fn[x]
                (incanter.stats/t-test (first x) :y (second x)))
              (partition 2
                         (nth  
                           (map #(partition bucket-size %1)
                                (exper-diff 
                                  exp-matrix))sample-num))))
    (map  #(vector (:p-value %) (:x-mean %))
         (map (fn[x]
                (incanter.stats/t-test (first x) :y (second x)))
              (partition 2
                         (rest (nth  
                                 (map #(partition bucket-size %1)
                                      (exper-diff 
                                        exp-matrix))sample-num)))))))

(defn total-counts-per-sample [experiment-mat]
  "Counts all active neurons for whole sample"
  (reduce 
    (fn[x y] 
      (map #(+ %1 %2) x y)) (exper-counts experiment-mat )))




  

(defn total-diff-per-sample [experiment-mat]
  "diff all active neurons for whole sample"
  (reduce 
    (fn[x y] 
      (map #(+ %1 %2) x y)) (exper-diff experiment-mat )))

(defn create-input-samples [num-samples act-size]
  "creates a vector of vector samples"
  (into [] (repeatedly num-samples
                       (fn [] (into [] 
                                    (repeatedly act-size (fn [] (rand-nth [1 0]))))))))

(defn view-sample-counts [exper-res]
  "takes activataion vecs of experiment which is experiment->sample->obervation [[[]]] -- map is not working correctly with graph"
  (let [exper-input (if (string? exper-res)
                      (read-string exper-res)
                      exper-res)
        exper-counts (map 
                       (fn[sample] (map #(apply + % ) sample)) exper-input)
        obs-length (count (first exper-counts))
        act-chart (chart/time-series-plot (range 1 (+ obs-length  1)) (repeat obs-length  0) :title "Activations at time")]
    (loop [exper-t exper-counts] 
      (if (empty? exper-t)
        (incant/view act-chart)
        (do
          (chart/add-lines act-chart 
                           (range 1 (+ 1 obs-length)) (first exper-t))
          (recur (rest exper-t)))))))

(defn view-sample-diff [exper-res]
  "takes activataion vecs of experiment which is experiment->sample->obervation [[[]]] -- map is not working correctly with graph"
  (let [exper-input (if (string? exper-res)
                      (read-string exper-res)
                      exper-res)
        exper-diff  (map
                      (fn [in] (map (fn [v] 
                                      (apply + (apply map #(java.lang.Math/abs (- %1 %2)) v)))
                                    (partition 2 in))) exper-input)
        obs-length (count (first exper-diff))
        act-chart (chart/time-series-plot (range 3 (+ obs-length  3)) (repeat obs-length  0) :title "Differences")]
    (loop [exper-t exper-diff] 
      (if (empty? exper-t)
        (incant/view act-chart)
        (do
          (chart/add-lines act-chart 
                           (range 1 (+ 1 obs-length)) (first exper-t))
          (recur (rest exper-t)))))))

(defn exper-inter-graph [experiment-vec]
  (let [exper-input (if (string? experiment-vec)
                      (read-string experiment-vec)
                      experiment-vec)
        x (range (count (ffirst exper-input)))]
   (incant/view 
     (chart/dynamic-scatter-plot [v (range (count (first exper-input)))
                             l (range (count  exper-input))]
                            (for [x-index x] [x-index (nth (nth (nth exper-input l) v) x-index)])
                            :title "Activation Per Unit time"))))