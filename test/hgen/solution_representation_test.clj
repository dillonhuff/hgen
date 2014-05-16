(ns hgen.solution-representation-test
	(:require [clojure.test :refer :all]
		[hgen.problem :refer :all]
		[hgen.solution-representation :refer :all]))

(def test-prob
	(make-problem
		'bool
		#{'* '> '+}
		#{'n 'm}
		{'bool ['>] 'int ['n 'm '+ '*]}
		{'> ['bool 'bool] '* ['bool 'bool] '+ ['int 'int]}
		(fn [population] 1)))

(deftest terminal-to-list
	(is (= [1] (code-tree-to-list 1))))