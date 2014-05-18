(ns hgen.gp-runner
	(:require [hgen.problem :refer :all]
		[hgen.solution-representation :refer :all]))

(defn make-generation
	[problem outcomes-and-cdf old-population new-population]
	(if (= (count new-population) (count old-population))
		new-population
		(let [father (rand-sample outcomes-and-cdf)
			mother (rand-sample outcomes-and-cdf)
			child (swap-code-lists father mother)]
			(recur problem outcomes-and-cdf old-population (conj new-population child)))))

(defn new-generation
	[problem old-population]
	(let [population-size (count old-population)
		pop-to-fitness (fitness problem old-population)
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