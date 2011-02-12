(**
  Module permettant de construire un individu plus évolué par algorithme génétique.
  Le but est d'augment le degré d'adaptation (fitness) par sélection, mutation et reproduction.
  
  L'algorithme générale est de la forme suivante :
   - génération aléatoire via generate_population, autour d'un individu
   - évaluation des individus
   - application des croisement et mutation (non implémentée)
   - sélection des individus
  (- on réitère l'algorithme à partir de l'étape 2)
  
  On construit donc un individu viable capable de réagir de façon plus précise aux taches demandées.
**)


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