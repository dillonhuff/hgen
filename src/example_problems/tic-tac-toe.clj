(ns example-problems.tic-tac-toe)

(defn new-board
	[]
	{1 0 2 0 3 0 4 0 5 0 6 0 7 0 8 0 9 0})

(defn get-loc
	[board location]
	(board location))

(defn empty-squares
	[board]
	(map first (filter (fn [ent] (= 0 (second ent))) board)))

(defn test-at
	[value board location]
	(= value (get-loc board location)))

(defn place-x
	[board location]
	(assoc board location 1))

(defn place-o
	[board location]
	(assoc board location -1))

(defn x-at?
	[board location]
	(test-at 1 board location))

(defn o-at?
	[board location]
	(test-at -1 board location))

(defn empty-at?
	[board location]
	(test-at 0 board location))

(defn value-at-all?
	[value board location-list]
	(every? (fn [location] (test-at value board location)) location-list))

(def tic-tac-toes [[1 2 3] [4 5 6] [7 8 9] [1 4 7] [2 5 8] [3 6 9] [1 5 9] [3 5 7]])

(defn team-wins?
	[board team]
	(some (fn [tic-tac] (value-at-all? team board tic-tac)) tic-tac-toes))

(defn x-wins?
	[board]
	(team-wins? board 1))

(defn o-wins?
	[board]
	(team-wins? board -1))

(defn rand-player
	[board]
	(rand-nth (empty-squares board)))

(defn show-square
	[board location]
	(let [loc-val (board location)]
		(if (= loc-val 1)
			" X "
			(if (= loc-val -1)
				" O "
				" . "))))

(defn show-row
	[row-num board]
	(let [row-off (* 3 (- row-num 1))
		p1 (+ row-off 1)
		p2 (+ row-off 2)
		p3 (+ row-off 3)]
	(str (show-square board p1) (show-square board p2) (show-square board p3) "\n")))

(defn show-board
	[board]
	(str (show-row 1 board) (show-row 2 board) (show-row 3 board)))

(defn play-game-with-output
	[board current-move x-player o-player]
	(if (x-wins? board)
		(print (str (show-board board) "X wins!\n"))
		(if (o-wins? board)
			(print (str (show-board board) "O Wins!\n"))
			(if (= 0 (count (empty-squares board)))
				(print (str (show-board board) "Cats Game!\n"))
				(if (= 1 current-move)
					(let [new-board (place-x board (x-player board))]
						(play-game new-board -1 x-player o-player))
					(let [new-board (place-o board (o-player board))]
						(play-game new-board 1 x-player o-player)))))))

(defn play-game
	[board current-move x-player o-player]
	(if (x-wins? board)
		1
		(if (o-wins? board)
			-1
			(if (= 0 (count (empty-squares board)))
				0
				(if (= 1 current-move)
					(let [new-board (place-x board (x-player board))]
						(play-game new-board -1 x-player o-player))
					(let [new-board (place-o board (o-player board))]
						(play-game new-board 1 x-player o-player)))))))