(ns hgen.problem)

; Description of problem to be submitted to
; genetic algorithm for processing

(defrecord Problem
	[solution-type
	functions
	terminals
	func-set
	term-set
	arg-types
	types-to-vocab
	vocab-to-types
	fitness-function])

(defn voc-to-types
	[type-to-voc]
	(let [vocab (second type-to-voc)]
		(into {} (map (fn [v] {v (first type-to-voc)}) vocab))))

(defn make-vocab-to-types
	[types-to-vocab]
	(reduce merge (map voc-to-types types-to-vocab)))

(defn make-problem
	[sol-type funcs terms types-to-voc arg-types fitness]
	(let [term-vec (vec terms)
		func-vec (vec funcs)]
		(Problem.
			sol-type
			func-vec
			term-vec
			(set funcs)
			(set terms)
			arg-types
			types-to-voc
			(make-vocab-to-types types-to-voc)
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

(defn rand-term-of-type
	[problem type]
	(let [vals-of-type (get (:types-to-vocab problem) type)]
		(rand-nth (filter (fn [t] (contains? vals-of-type t)) (:terminals problem)))))

(defn return-type
	[problem value]
	((:vocab-to-types problem) value))

(defn type-signature
	[problem function]
	((:arg-types problem) function))

(defn num-args
	[problem function]
	(count ((:arg-types problem) function)))

(defn is-func?
	[problem value]
	(contains? (:func-set problem) value))

(defn fitness
	[problem population]
	((:fitness problem) population))

(defn rand-term-args
	[problem types]
	(mapcat list (map rand-term-of-type types)))

(defn rand-init-individual
	[problem]
	(let [rand-root (rand-of-type problem (solution-type problem))]
		(if (is-func? problem rand-root)
			(conj (rand-term-args problem (type-signature rand-root)) rand-root)
			(rand-init-individual problem))))