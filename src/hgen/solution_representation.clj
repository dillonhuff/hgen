(ns hgen.solution-representation
	(:require [hgen.problem :refer :all]))

; This module handles how candidate solutions in
; the population are generated, represented, mutated and reproduced

(defn code-tree-to-list
	[code-tree]
	(if (seq? code-tree)
		(mapcat code-tree-to-list code-tree)
		[code-tree]))