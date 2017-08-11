;; From/about Bruce Glymour's "Modeling Environments: Interactive
;; Causation and Adaptations to Environmental Conditions", 2011
;; Philosophy of Science.

(ns popgen.glymour2011)

(defn p'
  "p' wa1 wa2 wb1 wb2 p1 q1 p = expected frequency of A in the next
  generation given frequency p in this generation, proportion p1 of
  A's that are in environment E1 rather than E2, proportion q1 of
  B's that are in E1 rather than E2, and absolute fitnesses wa1 and 
  wa2 for A in E1 and E2 respectively, and absolute fitnesses wb1 and 
  wb2 for B in E1 and E2."
  [wa1 wa2 wb1 wb2 p1 q1 p]
  (let [q (- 1 p)
        p2 (- 1 p1)
        q1 (- 1 q1)
        abs-p' (* p (+ (* wa1 p1) (wa2 p2)))
        abs-q' (* q (+ (* wa1 q1) (wa2 q2)))]
    (/ abs-p'(+ abs-p' abs-q'))))

;; Equation 1, p. 455
(def eq1-p' (partial p' 97 10 100 7 0.5 0.5))

