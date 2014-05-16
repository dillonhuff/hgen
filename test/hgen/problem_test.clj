(ns hgen.problem-test
	(:require [clojure.test :refer :all]
		[hgen.problem :refer :all]))

(def test-prob
	(make-problem
		'bool
		#{'and 'or '+}
		#{'a 'b 'c 'n 'm}
		{'a 'bool 'b 'bool 'c 'bool 'n 'int 'm 'int 'and 'bool 'or 'bool '+ 'int}
		{'and ['bool 'bool] 'or ['bool 'bool] '+ ['int 'int]}
		(fn [population] 1)))

(deftest rand-bool-term-is-bool
	(testing "Randomly selected boolean isn't actually boolean"
		(is (= 'bool (return-type test-prob (rand-of-type test-prob 'bool))))))