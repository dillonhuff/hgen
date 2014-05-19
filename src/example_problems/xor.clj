(ns example-problems.xor
	(:require [hgen.problem :refer :all]
		[hgen.solution-representation :refer :all]
		[hgen.gp-runner :refer :all]
		[hgen.utilities :refer :all]))

; Simple test problem to demonstrate the system

(defn correct-xor
	[d0 d1]
	(and (not (and d0 d1)) (or d0 d1)))

(def test-cases [[true true] [true false] [false true] [false false]])

(def correct-answers [false true true false])

(defn ind-fitness
	"Measures absolute fitness of a single individual"
	[individual]
	(let [guesses (map (fn [d0-d1] (individual (first d0-d1) (second d0-d1))) test-cases)
		num-correct (reduce + (map bool-to-num guesses))]
		(/ num-correct (float (count test-cases)))))

(defn population-fitness
	[population]
	(let [candidate-solutions (map (fn [code] (eval (make-func '[d0 d1] code))) population)
		ind-fitnesses (map ind-fitness candidate-solutions)]
		(normalized-fitness ind-fitnesses)))

(def xor-problem
	(make-problem
		'bool
		#{'and 'or 'not}
		#{'d0 'd1}
		{'bool #{'and 'or 'not 'd0 'd1}}
		{'and ['bool 'bool] 'or ['bool 'bool] 'not ['bool]}
		population-fitness))

(defn run-xor
	[population-size number-of-generations]
	(evolve-program xor-problem population-size number-of-generations))