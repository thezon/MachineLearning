(ns projectESN.network
    (:require [clojure.core.matrix :as m]
              [projectESN.support :as sup]))


(def act-threshold 0.99)

(defn eval-neuron-weight [act-sum]
  "Simple activation function"
  (if (> (java.lang.Math/tanh act-sum) act-threshold)
    1
    0))

(defn sum-neuron-weights [node-num conn-loc-vec network]
  "Gets sum of weights that are active." 
  ;(m/length
    (apply +
      (replace
      (nth network node-num) conn-loc-vec)))

(defn neuron-loc-by-state [act-vector state-key]
  "gets locations from activation vector by state-key: :active :inactive :all"
  (into []
        (cond 
          (= state-key :active)
          (sup/loc-by-value 1 act-vector)
          (= state-key :inactive)
          (sup/loc-by-value 0 act-vector)
          (= state-key :all)
          (range (count act-vector)))))


(defn set-diagonal [neur-mat is-active?]
  "sets the diagonal to zero or one"
  (m/sub neur-mat 
         (m/diagonal-matrix 
           (if is-active?
             (map dec (m/diagonal neur-mat))
             (m/diagonal neur-mat)))))

(defn createRandNetwork [num-neur con-perc normalize?]
  "Creates a neuro network matrix of size N with connection with probablity connect-perc"
  (into [] 
        (repeatedly num-neur
                    #(into []
                           (let [rand-vec (repeatedly num-neur
                                                      (fn [] (sup/rand-val con-perc)))]
                             (if normalize?
                               (m/normalise rand-vec)
                               rand-vec))))))


;not valid should removed
(defn ESN-base-network [num-neur]
  "Creates network that is fully connected with randomized weight and dialgonal zero"
	  (set-diagonal 
     (createRandNetwork num-neur 1 true)
    false))

(defn update-network [network act-v] 
  "Goes through network one time and sets activations"
  (let [size-v (count act-v)
        act-loc (neuron-loc-by-state act-v :active)]
    (loop [ counter 0 new-act act-v]
      (if (<= size-v counter)
        new-act
        (recur 
          (inc counter) 
          (into []
                (concat 
                  (subvec new-act 0 counter )
                  [(eval-neuron-weight (sum-neuron-weights counter act-loc network))]
                  (subvec new-act  (inc counter ) (count new-act)))))))))

(defn run-model [times network activ-vector]
  (loop [counter 0 act-history [activ-vector]]
    (if (>= counter times)
      act-history 
      (do (sup/log-run "model activation  " counter " " (last act-history))
        (recur (inc counter)
               (into [] (concat act-history [(update-network network (last act-history))])))))))
