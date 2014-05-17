(ns hgen.gp-runner
	(:require [hgen.solution-representation :refer :all]))

(defn make-generation
	[problem outcomes-and-cdf pop-size pop-so-far]
	(if (= (count pop-so-far) pop-size)
		pop-so-far
		(let [father ()])))

(defn new-generation
	[problem old-population]
	(let [population-size (count old-population)
		outcomes-and-cdf ()]
		(make-generation problem population-size [])))