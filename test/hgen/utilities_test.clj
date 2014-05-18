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

(deftest make-add-func
	(testing "add two numbers function not made correctly"
		(is (= 5 ((eval (make-func ['x 'y] '(+ x y))) 2 3)))))

(deftest normalized-fitness-one-ind
	(testing "one items fitness is changed by normalized fitness"
		(is (= {:a 1.0} (normalized-fitness {:a 1.0})))))

(deftest normalized-fitness-many-ind
	(testing "several individuals normalized fitness computed wrong"
		(is (= {:a (/ 3.5 7.0) :b (/ 1.2 7.0) :c (/ 2.3 7.0)} (normalized-fitness {:a 3.5 :b 1.2 :c 2.3})))))