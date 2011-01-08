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

let min_en_norme a b =
	if abs_float a < abs_float b then a else b

module Random = struct
	include Random
	
	let float_range min max =
		min +. (max-.min) *. Random.float 1.
end


module Array = struct
	include Array
	
	let iter2 f a1 a2 =
  		if Array.length a1 <> Array.length a2
  			then raise (Invalid_argument "Array.iter2");
  		for i = 0 to Array.length a1 - 1 do
    		f a1.(i) a2.(i);
  		done
end

(* En millisecondes ! *)
let wait time =
    ignore(Unix.select [] [] [] (float_of_int time /. 1000.))