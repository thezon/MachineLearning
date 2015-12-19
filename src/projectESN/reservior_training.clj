(ns projectESN.reservior-training
  (:require [clojure.core.matrix :as m]
            [projectESN.support :as sup]
            [projectESN.network :as net]))

(def lr-AA  -0.1)
(def lr-AI -10)
(def lr-IA 10)

(def impulse? false)

(defn adjust-weights [neur-num network act-v learn-rate active?]
  "adjusts all active links (including itself) by learning raing * weight + weight"
  (let [result (into []
                     (map #(* %1 %2)
                          (map #(* learn-rate %) 
                               (if active?
                                 act-v
                                 (map (fn[x] (if (= x 1) 0 1)) act-v)))
                          (nth network neur-num)))
        _ (sup/log-run "adjust-weights" result)]
    result))

(defn train-neuron [neur-num network act-v] 
  (into []
        (if (= (nth act-v neur-num) 1)
          (do (sup/log-run "Train-neuron: active adjust")
            ;(m/normalise
              (map (fn [x y z] (+ x y z));rounding error < 10^-16
                   (adjust-weights neur-num network act-v lr-AA true)
                   (adjust-weights neur-num network act-v lr-AI false)
                   (nth network neur-num)));)
          (do (sup/log-run "Train-neuron: inactive adjust")
           ; (m/normalise
              (map (fn [x y] (+ x y))
                   (adjust-weights neur-num network act-v lr-IA true) 
                   (nth network neur-num))))));)

(defn train-cycle-network [network act-v]
  "Performs one training cylce and returns a new network"
  (let [size-v (count act-v)]
    (loop [counter 0 new-network network]
      (if (<= size-v counter)
        new-network
        (recur
          (inc counter)
          (into []
                (concat
                  (subvec new-network 0  counter)
                  [(train-neuron counter network act-v)]
                  (subvec new-network (inc counter) (count network)))))))))

(defn train-network [network act-vec num-cycles ];needs to be named as impress-network
  "trains network for n cycles. If impluse then activation results is applyed to next cycle 
Otherwise act vec is impressed on network for num-cycles"
  "Impresses network with a activation vector"
  (loop [counter 0  new-network network act-vec-in act-vec ]
    (if (<= num-cycles counter)
      new-network
      (do
        (sup/log-run "train-network activiation: " 
                     (net/update-network new-network act-vec))
        (recur (inc counter)
               (train-cycle-network network  act-vec-in)
               (if impulse?
                 (net/update-network new-network act-vec-in)
                 act-vec-in ))))))
  


(defn boot-strap-trainer [network training-nodes boot-strap-num num-cycles]
  (loop [counter 0 new-network network]
    (if (<= boot-strap-num counter)
      new-network
      (do (sup/log-run "New bootstrap")
        (recur (inc counter) (train-network network training-nodes num-cycles))))))

(defn boot-strap-train-rand-act [network boot-strap-num num-cycles]
  (loop [samples (sup/create-input-samples boot-strap-num (count network)) new-network network]
    (if (empty? samples)
      new-network
      (do (sup/log-run "New bootstrap")
        (recur (rest samples) 
               (train-network  new-network (first samples) num-cycles))))))