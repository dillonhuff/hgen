(ns hgen.problem)

; Description of problem to be submitted to
; genetic algorithm for processing

(defrecord Problem
	[solution-type
	functions
	terminals
	return-types
	arg-types
	types-to-vocab
	fitness-function])

(defn make-problem
	[sol-type funcs terms ret-types arg-types fitness]
	(let [term-vec (vec terms)
		func-vec (vec funcs)
		vocab (concat term-vec func-vec)]
		(Problem.
			sol-type
			funcs
			terms
			ret-types
			arg-types
			(make-types-to-vocab vocab ret-types)
			fitness)))

(defn solution-type
	[problem]
	(:solution-type problem))

(defn rand-function
	[problem]
	(rand-nth (:functions problem)))

(defn rand-terminal
	[problem]
	(rand-nth (:terminals problem)))

(defn rand-of-type
	[problem type]
	(rand-nth ((:types-to-vocab problem) type)))

(defn return-type
	[problem value]
	((:return-types problem) value))

(defn num-args
	[problem function]
	(count ((:arg-types problem) function)))