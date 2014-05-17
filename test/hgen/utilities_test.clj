(ns hgen.utilities-test
	(:require [clojure.test :refer :all]
		[hgen.utilities :refer :all]))

(deftest outcomes-and-probs-simple
	(testing "one outcome cdf failed"
		(is (= {:outcomes [3] :cdf [1.0]} (outcome-prob-map-to-outcomes-cdf {3 1.0})))))

(deftest outcomes-and-probs-many-outcomes
	(testing "multiple outcome cdf failed"
		(is (= 
			{:outcomes [1 4 6 9] :cdf [0.2 0.5 0.75 1.0]}
			(outcome-prob-map-to-outcomes-cdf {1 0.2 4 0.3 6 0.25 9 0.25})))))