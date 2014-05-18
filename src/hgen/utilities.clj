(ns hgen.utilities)

; Utilities for sampling outcomes at random from a probability distribution

(defn mk-cdf
	[cdf-so-far probs ind]
	(if (>= ind (count probs))
		cdf-so-far
		(let [prob-so-far (nth cdf-so-far (- ind 1))
			next-prob (nth probs ind)]
			(recur (conj cdf-so-far (+ prob-so-far next-prob)) probs (+ ind 1)))))

(defn make-cdf
	[probs]
	(mk-cdf [(first probs)] probs 1))

(defn outcome-prob-map-to-outcomes-cdf
	[outcome-prob-map]
	(let [as-vec (into [] outcome-prob-map)
		outcomes (map first as-vec)
		probs (map second as-vec)]
		{:outcomes outcomes :cdf (make-cdf probs)}))

(defn r-sample
	[cum-val cum-cdf ind cdf]
	(if (>= cum-cdf cum-val)
		ind
		(recur cum-val (+ (nth cdf ind) cum-cdf) (+ ind 1) cdf)))

(defn rand-sample
	"Sample from a vector according to probability"
	[outcomes-to-cdf]
	(let [outcomes (outcomes-to-cdf :outcomes)
		cdf (outcomes-to-cdf :cdf)])
	(nth outcomes (r-sample (rand 1.0) (first cdf) 0 cdf)))

; Map utilities

(defn sort-map-by-value
	[map-to-sort]
	(into (sorted-map-by
		(fn [key1 key2]
			(let [val1 (map-to-sort key1)
				val2 (map-to-sort key2)]
				(if (< val1 val2)
					1
					-1))))
		map-to-sort))