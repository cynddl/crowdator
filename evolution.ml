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
		let choose_best (stathme: M.t->int) pop =
		    let rec aux = function
			    | [] -> failwith "Empty population from Evoluate.choose_best"
			    | [a] ->
			        (a, stathme a)
			    | hd :: tl ->
			        let r_hd = stathme hd in
				    if r_hd = 0 then
				        (hd, r_hd)
			        else
				        (
				            let b, r_b = aux tl in
				            if r_hd < r_b then
				                (hd, r_hd)
				            else
				                (b, r_b)
			            )
			in
			    fst (aux pop)
		
		let elect_one one f nmax func =
		    let pop = generate_population one f nmax in
		    choose_best func pop

	end

module HopfieldEvoluate = Evoluate (Hopfield)