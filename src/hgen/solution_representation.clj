(ns hgen.solution-representation
	(:require [hgen.problem :refer :all]))

; This module handles how candidate solutions in
; the population are generated, represented, mutated and reproduced

; Code for manipulating list and tree representations of solutions

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
		(let [head (nth code-list ind)
			inc-size (+ size-so-far 1)
			dec-args (- args-left 1)]
			(if (is-func? problem head)
				(recur problem code-list (+ ind 1) (+ dec-args (num-args problem head)) inc-size)
				(recur problem code-list (+ ind 1) dec-args inc-size)))))

(defn subtree-size
	[problem code-list subtree-start-index]
	(let [head (nth code-list subtree-start-index)]
		(if (is-func? problem head)
			(total-subtree-size problem code-list (+ 1 subtree-start-index) (num-args problem head) 1)
			1)))

; Code for crossover reproduction

(defn break-into-start-swap-end
	[problem code-list swap-location]
	(let [start-to-swap-end (split-at swap-location code-list)
		start (first start-to-swap-end)
		swap-region-size (subtree-size problem code-list swap-location)
		swap-and-end (split-at swap-region-size (second start-to-swap-end))]
		{:start start :swap (first swap-and-end) :end (second swap-and-end)}))

(defn swap-segmented-code
	[swap-recipient swap-giver]
	(list-to-code-tree (concat (swap-recipient :start) (swap-giver :swap) (swap-recipient :end))))

(defn swap-code-lists
	[father-list mother-list]
	(let [father-swap-location (rand-int (count father-list))
		mother-swap-location (rand-int (count father-list))
		father-code-regions (break-into-start-swap-end father-list father-swap-location)
		mother-code-regions (break-into-start-swap-end mother-list mother-swap-location)]
		{:father-child (swap-segmented-code father-code-regions mother-code-regions)
			:mother-child (swap-segmented-code mother-code-regions father-code-regions)}))