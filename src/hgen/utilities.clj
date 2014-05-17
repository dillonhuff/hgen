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