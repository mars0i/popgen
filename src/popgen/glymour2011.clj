;; From/about Bruce Glymour's "Modeling Environments: Interactive
;; Causation and Adaptations to Environmental Conditions", 2011
;; Philosophy of Science.

(ns popgen.glymour2011
  (:require [infix.macros :refer [$=]])) ; infix macro $=

;(defmacro $= 
;  "Abbreviation for the infix macro."
;  [& expr]
;  `(infix ~@expr))

;; convenience abbrievation
(def ptl partial)

(defn next-p
  "next-p wA1 wA2 wB1 wB2 p1 q1 p = expected frequency of A in the next
  generation given frequency p in this generation, proportion p1 of
  A's that are in environment E1 rather than E2, proportion q1 of
  B's that are in E1 rather than E2, and absolute fitnesses wA1 and 
  wA2 for A in E1 and E2 respectively, and absolute fitnesses wB1 and 
  wB2 for B in E1 and E2."
  [p p1 q1 wA1 wA2 wB1 wB2]
  (let [q  ($= 1 - p)
        p2 ($= 1 - p1)
        q2 ($= 1 - q1)
        abs-p' ($= p * (wA1 * p1  +  wA2 * p2))
        abs-q' ($= q * (wA1 * q1  +  wA2 * q2))]
    ($= abs-p' / (abs-p' + abs-q'))))

(defn next-p* 
  "Just like next-p but groups params: 
  subenv-freqs = [p1 q1], abs-fitns = [wA1 wA2 wB1 wB2]."
  [p subenv-freqs abs-fitns]
  (let [[p1 q1] subenv-freqs
        [wA1 wA2 wB1 wB2] abs-fitns]
    (next-p p p1 q1 wA1 wA2 wB1 wB2)))

;; Parameters for next-p in equation 1, p. 455:
(def eq1-subenv-freqs [0.5 0.5])
(def eq1-abs-fitns [97 10 100 7])
(def eq1-params (concat eq1-subenv-freqs eq1-abs-fitns))

;; Equation 1, p. 455 as a function:
(defn eq1-next-p [p]
  (apply next-p p eq1-params))


;; old prefix notation version
;(defn prefix-next-p
;  "next-p wA1 wA2 wB1 wB2 p1 q1 p = expected frequency of A in the next
;  generation given frequency p in this generation, proportion p1 of
;  A's that are in environment E1 rather than E2, proportion q1 of
;  B's that are in E1 rather than E2, and absolute fitnesses wA1 and 
;  wA2 for A in E1 and E2 respectively, and absolute fitnesses wB1 and 
;  wB2 for B in E1 and E2."
;  [p p1 q1 wA1 wA2 wB1 wB2]
;  (let [q  (- 1 p)
;        p2 (- 1 p1)
;        q2 (- 1 q1)
;        abs-p' (* p (+ (* wA1 p1) (* wA2 p2)))
;        abs-q' (* q (+ (* wA1 q1) (* wA2 q2)))]
;    (/ abs-p'(+ abs-p' abs-q'))))
