(* Couche de neurones inter-connectés selon le principe du 
 * réseau de Hopfield : matrice symétrique, diagonale nulle *)

open MatrixHopfield
open Primitives

class mesh _n _func =
object (s)
	val n = _n
	val mutable weights = Matrix.make_matrix _n _n 0.
	val mutable etats = Matrix.make_matrix _n 1 0.
	val activation_func = _func

	method get_weights = weights
	method set_weights (w:MatrixHopfield.t) = weights <- w


	method get_state = etats
	method set_state e = etats <-e


	method get_activation_func = activation_func


    (* Met à jour de manière itérative les états et les poids du mesh. *)
    
	method randomise mini maxi =
		weights <- MatrixHopfield.random_range n n mini maxi;

		for i = 0 to n - 1 do
			etats.(i).(0) <- 2. *. float_of_int (Random.int 2) -. 1.
		done


	(* Mise à jour des poids par règle de Hebb *)
	(* La matrice reste bien symétrique ! *)

	method private update_weights =
		for i = 0 to n-1 do
			for j = 0 to n-1 do
				if i <> j && etats.(i).(1) = 1. && etats.(j).(1) = 1. then
					weights.(i).(j) <- weights.(i).(j) +. 0.1
			done
		done


	(* Mise à jour synchrone des états *)
	
	method update_states =
		let mat = Matrix.mult weights etats in
		Matrix.apply mat activation_func;
		etats <- mat


	method process input =
		for i = 0 to Array.length input - 1 do
			etats.(i).(0) <- input.(i).(0)
		done;		
		s#update_states
end


(* Affiche un mesh : matrice des poids et matrice unicolonne des états *)
let print_mesh mesh =
	Matrix.print mesh#get_weights;
	Matrix.print mesh#get_state


(* Copie un mesh : la copie des états importe peu *)
let copy m =
	let weights = Matrix.clone m#get_weights in
	let n = Array.length weights in
	(* let state = Matrix.clone m#get_state in*)
	let mi = new mesh n m#get_activation_func in
	(*	mi#set_state state;*)
	mi#set_weights weights;
	mi
