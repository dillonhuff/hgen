(ns hgen.utilities)

(defn rand-sample
	"Sample from a vector according to probability"
	[cdf]
	(r-sample (rand 1.0) 0.0 0 probabilities))

(defn r-sample
	[cum-val cum-cdf ind cdf]
	(if (>= cum-cdf cum-val)
		ind
		(recur cum-val (+ (nth cdf ind) cum-cdf) (+ ind 1) cdf)))

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