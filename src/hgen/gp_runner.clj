(ns hgen.gp-runner
	(:require [hgen.problem :refer :all]
		[hgen.solution-representation :refer :all]
		[hgen.utilities :refer :all]))

(defn make-generation
	[problem outcomes-and-cdf old-population new-population]
	(if (= (count new-population) (count old-population))
		new-population
		(let [father (rand-sample outcomes-and-cdf)
			mother (rand-sample outcomes-and-cdf)
			offspring (swap-code-lists problem father mother)
			father-child (:father-child offspring)
			mother-child (:mother-child offspring)]
			(recur problem outcomes-and-cdf old-population (conj (conj new-population mother-child) father-child)))))

(defn new-generation
	[problem old-population]
	(let [pop-to-fitness (into {} (map vector old-population (fitness problem (pmap (partial list-to-code-tree problem) old-population))))
		outcomes-and-cdf (outcome-prob-map-to-outcomes-cdf pop-to-fitness)]
		(make-generation problem outcomes-and-cdf old-population [])))

(defn rand-pop
	[problem pop-size cur-pop]
	(if (= pop-size (count cur-pop))
		cur-pop
		(recur problem pop-size (conj cur-pop (rand-init-individual problem)))))

(defn rand-init-population
	[problem population-size]
	(rand-pop problem population-size []))

(defn run-gp
	[problem population max-generations cur-gen-num]
	(if (= max-generations cur-gen-num)
		population
		(let [next-generation (new-generation problem population)
			next-gen-num (+ cur-gen-num 1)]
			(recur problem next-generation max-generations next-gen-num))))

(defn evolve-program
	[problem population-size num-generations]
	(let [init-pop (rand-init-population problem population-size)]
		(run-gp problem init-pop num-generations 0)))