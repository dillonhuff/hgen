(ns hgen.problem-test
	(:require [clojure.test :refer :all]
		[hgen.problem :refer :all]))

(def test-prob
	(make-problem
		'bool
		#{'and 'or '+}
		#{'a 'b 'c 'n 'm}
		{'bool ['a 'b 'c 'and 'or] 'int ['n 'm '+]}
		{'and ['bool 'bool] 'or ['bool 'bool] '+ ['int 'int]}
		(fn [population] 1)))

(deftest rand-bool-term-is-bool
	(testing "Randomly selected boolean isn't actually boolean"
		(is (= 'bool (return-type test-prob (rand-of-type test-prob 'bool))))))

(deftest funcs-have-correct-num-args
	(testing "+ doesn't have 2 args"
		(is (= 2 (num-args test-prob '+)))))