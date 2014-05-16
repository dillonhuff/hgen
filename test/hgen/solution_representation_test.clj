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
	(testing "terminal list failed"
		(is (= [1] (code-tree-to-list 1)))))

(deftest tree-to-list
	(testing "complicated tree to code list failed"
		(is (= ['+ '- 'x 'y '* 'y 3] (code-tree-to-list '(+ (- x y) (* y 3)))))))