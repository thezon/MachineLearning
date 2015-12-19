(ns projectESN.core
  (:require [projectESN.network :as net]
            [projectESN.support :as sup]
            [projectESN.reservior-training :as tr]
            [projectESN.networkTest :as test]
            [incanter.core :as incant]
            [incanter.charts :as chart]))


(def net-size 100)
(def observation-time 100)
(def network-connectivity 1)
(def training-random-impressions 50)
(def training-impression-time 5)
(def input (repeatedly net-size #(rand-nth [1 0])))
(def num-samples 10)

(defn train-ESN-reservoir [reservoir] 
  (tr/boot-strap-train-rand-act  
    reservoir 
    training-random-impressions 
    training-impression-time))

(defn create-rand-reservoir []
  (net/createRandNetwork net-size network-connectivity true))

(defn create-input-samples []
  (test/create-input-samples num-samples  net-size))
  
  
(defn observe-network [reservoir sample]
  "One model run given a sample vector"
  (net/run-model 
    observation-time 
    reservoir
    sample))

 (defn analyze-network[sample-vec reservoir-mat]
   "returns actitivation vector for experiment with samples for each time unit"
     (map #(observe-network reservoir-mat %) sample-vec))

(comment "helpful functions for analysis"
  
(def network (create-rand-reservoir))
(def sample (create-input-samples))
(def control (analyze-network sample network))
(def experiment (analyze-network sample (train-ESN-reservoir network)))

(projectESN.networkTest/view-sample-counts control)
(projectESN.networkTest/view-sample-counts experiment)

(projectESN.networkTest/view-sample-diff control)
(projectESN.networkTest/view-sample-diff experiment)

(projectESN.networkTest/exper-inter-graph control)
(projectESN.networkTest/exper-inter-graph experiment)

(def sim-net-1 (sup/get-file-datastr "sim-net-1.txt"))


(test/t-test 
  (test/total-counts-per-sample control) 
  (test/total-counts-per-sample experiment))

(test/t-test 
  (test/total-diff-per-sample control) 
  (test/total-diff-per-sample experiment))

;test stat
(incanter.stats/sd 
  (test/total-diff-per-sample experiment))
)
