;; From/about Bruce Glymour's "Modeling Environments: Interactive
;; Causation and Adaptations to Environmental Conditions", 2011
;; Philosophy of Science.

(ns popgen.glymour2011
  (:require [infix.macros :refer [infix]]))

(defmacro $ 
  "Abbreviation for the infix macro."
  [& expr]
  `(infix ~@expr))

(defn p'
  "p' wA1 wA2 wB1 wB2 p1 q1 p = expected frequency of A in the next
  generation given frequency p in this generation, proportion p1 of
  A's that are in environment E1 rather than E2, proportion q1 of
  B's that are in E1 rather than E2, and absolute fitnesses wA1 and 
  wA2 for A in E1 and E2 respectively, and absolute fitnesses wB1 and 
  wB2 for B in E1 and E2."
  [p p1 q1 wA1 wA2 wB1 wB2]
  (let [q  ($ 1 - p)
        p2 ($ 1 - p1)
        q2 ($ 1 - q1)
        abs-p' ($ p * (wA1 * p1  +  wA2 * p2))
        abs-q' ($ q * (wA1 * q1  +  wA2 * q2))]
    ($ abs-p' / (abs-p' + abs-q'))))

;; Parameters for p' in equation 1, p. 455:
(def eq1-params [0.5 0.5 97 10 100 7])

;; Equation 1, p. 455 as a function:
(defn eq1-p' [p]
  (apply p' p eq1-params))


;; old prefix notation version
(defn prefix-p'
  "p' wA1 wA2 wB1 wB2 p1 q1 p = expected frequency of A in the next
  generation given frequency p in this generation, proportion p1 of
  A's that are in environment E1 rather than E2, proportion q1 of
  B's that are in E1 rather than E2, and absolute fitnesses wA1 and 
  wA2 for A in E1 and E2 respectively, and absolute fitnesses wB1 and 
  wB2 for B in E1 and E2."
  [p p1 q1 wA1 wA2 wB1 wB2]
  (let [q  (- 1 p)
        p2 (- 1 p1)
        q2 (- 1 q1)
        abs-p' (* p (+ (* wA1 p1) (* wA2 p2)))
        abs-q' (* q (+ (* wA1 q1) (* wA2 q2)))]
    (/ abs-p'(+ abs-p' abs-q'))))
