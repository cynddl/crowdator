(* Vérifie si un prédicat est vrai sur toute une liste *)
let assume predicate l =
	let rec aux = function
		| [] -> true
		| hd :: tl -> (predicate hd) && aux tl
	in aux l

(* Vérifie si un prédicat est vrai au moins une fois sur une liste *)
let assume_once predicate l =
	let rec aux = function
		| [] -> false
		| hd :: tl -> (predicate hd) || aux tl
	in aux l

(* Vérifie si une valeur est comprise dans un intervalle *)
let is_in_range a n p =
	(a<=n && a>=p) || (a<=p && a>=n)

let min3 a b = min (min a b)