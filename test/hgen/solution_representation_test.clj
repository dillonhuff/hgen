(ns hgen.solution-representation-test
	(:require [clojure.test :refer :all]
		[hgen.problem :refer :all]
		[hgen.solution-representation :refer :all]))

(def test-prob
	(make-problem
		'bool
		#{'* '> '+ 'not}
		#{'n 'm}
		{'bool ['> 'not] 'int ['n 'm '+ '*]}
		{'> ['bool 'bool] 'not ['bool] '* ['bool 'bool] '+ ['int 'int]}
		(fn [population] 1)))

(deftest one-item-list-to-tree
	(testing "single item list to code tree"
		(is (= 4 (list-to-code-tree test-prob [4])))))

(deftest multi-item-tree
	(testing "test-prob code list to tree"
		(is (= '(not (> (+ n m) (* n m))) (list-to-code-tree test-prob ['not '> '+ 'n 'm '* 'n 'm])))))

(deftest terminal-subtree-size
	(testing "terminal subtree does not have size 1"
		(is (= 1 (subtree-size test-prob ['not 'x] 1)))))

(deftest unary-func-subtree-size
	(testing "unary func subtree does not have size 2"
		(is (= 2 (subtree-size test-prob ['not 'x] 0)))))

(deftest complicated-func-subtree-size-1
	(testing "complicated subtree has wrong size"
		(is (= 8 (subtree-size test-prob ['not '> '+ 'n 'm '* 'n 'not 'x] 1)))))

(deftest complicated-func-subtree-size-2
	(testing "complicated subtree has wrong size"
		(is (= 3 (subtree-size test-prob ['not '> '+ 'n 'm '* 'n 'not 'x] 2)))))

(deftest swap-location-is-start
	(testing "split at start not correct"
		(is (= ['not '> 'n 'm] (:swap (break-into-start-swap-end test-prob ['not '> 'n 'm] 0))))))

(deftest swap-location-is-end
	(testing "split at end was not correct"
		(is (= ['n] (:swap (break-into-start-swap-end test-prob ['+ '* 'n 'n '+ 'm 'n] 6))))))

(deftest swap-location-is-mid
	(testing "split in middle was not correct"
		(is (= ['+ 'n 'm] (:swap (break-into-start-swap-end test-prob ['> 'n '+ '+ 'n 'm 'n] 3))))))

(deftest terminal-to-list
	(testing "terminal list failed"
		(is (= [1] (code-tree-to-list 1)))))

(deftest tree-to-list
	(testing "complicated tree to code list failed"
		(is (= ['+ '- 'x 'y '* 'y 3] (code-tree-to-list '(+ (- x y) (* y 3)))))))