(ns hgen.solution-representation
	(:require [hgen.problem :refer :all]))

; This module handles how candidate solutions in
; the population are generated, represented, mutated and reproduced

(defn code-tree-to-list
	[code-tree]
	(if (seq? code-tree)
		(mapcat code-tree-to-list code-tree)
		[code-tree]))

(defn rev-list-to-code-tree
	[problem rev-code-list args]
	(if (= 0 (count rev-code-list))
		(first args)
		(let [head (first rev-code-list)
			tail (rest rev-code-list)]
			(if (is-func? problem head)
				(let [n-args (num-args problem head)]
					(recur problem tail (cons (cons head (take n-args args)) (drop n-args args))))
				(recur problem tail (cons head args))))))

(defn list-to-code-tree
	[problem code-list]
	(rev-list-to-code-tree problem (reverse code-list) ()))

(defn total-subtree-size
	[problem code-list ind args-left size-so-far]
	(if (= 0 args-left)
		size-so-far
		(let [head (nth code-list ind)]
			(if (is-func? problem head)
				(recur problem code-list (+ ind 1) (- (+ args-left (num-args problem head)) 1) (+ size-so-far 1))
				(recur problem code-list (+ ind 1) (- args-left 1) (+ size-so-far 1))))))

(defn subtree-size
	[problem code-list subtree-start-index]
	(let [head (nth code-list subtree-start-index)]
		(if (is-func? problem head)
			(total-subtree-size problem code-list (+ 1 subtree-start-index) (num-args problem head) 1)
			1)))