open Hopfield
open MatrixHopfield


module type MUTATOR = 
	sig
		type t
		val clone : t -> t
		val mutate : t -> float -> t
	end

module Evoluate = functor (M : MUTATOR) ->
	struct
	
		let rec generate_population one f = function
			| 0 -> [M.clone one]
			| n -> (M.mutate one f) :: (generate_population one f (n-1))

		(* On minimise le stathme *)
		let rec choose_best (stathme: M.t->int) = function
			| [] -> failwith "Empty population from Evoluate.choose_best"
			| [a] -> a
			| hd::tl ->
				let b = choose_best stathme tl in
				if stathme hd < stathme b then hd else b

	end

module HopfieldEvoluate = Evoluate (Hopfield)