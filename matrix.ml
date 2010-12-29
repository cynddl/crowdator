open Primitives

type t = float array array

(* Args: nb de lignes, ~ de colonnes, et élément principal *)
let make_matrix = Array.make_matrix

(* Génère une matrice aléatoire de floats *)
let random h w max =
	Array.init h (fun _ ->
		Array.init w (fun _ ->
			Random.float max
		)
	)

(* Applique une fonction à tous les éléments d'une matrice. TRANSFORMATION ! *)
let apply mat f =
	for i = 0 to (Array.length mat) - 1 do
		for j = 0 to (Array.length mat.(i)) - 1 do
			mat.(i).(j) <- f (mat.(i).(j))
		done
	done

let clone mat =
	let n = Array.length mat in
	let m = Array.length mat.(0) in
	Array.init n (fun i -> Array.init m (fun j -> mat.(i).(j)))

let map mat f =
	let h = Array.length mat in
	let w = Array.length mat.(0) in
	Array.init h (fun i ->
		Array.init w (fun j ->
			f(mat.(i).(j))
		)
	)

(* Transforme un vecteur en matrice colonne *)
let make_unicol x =
	let n = Array.length x in
	Array.init n (fun i -> Array.make 1 x.(i))

(* Multiplication naive *)
let mult (a:t) (b:t) =
	let h1,h2,w1,w2 = Array.length a, Array.length b, Array.length a.(0), Array.length b.(0) in
	assert (w1 == h2);
	let mat = make_matrix h1 w2 0. in
	for i = 0 to h1 - 1 do
		for j = 0 to w2 - 1 do
			let sum = ref 0. in
			for k = 0 to w1 - 1 do
				sum := !sum +. a.(i).(k) *. b.(k).(j);
			done;
			mat.(i).(j) <- !sum
		done
	done;
	mat
 
let print mat =
	let h = Array.length mat in
	let w = Array.length mat.(0) in
	let s = ref "" in
	for i = 0 to h-1 do
		for j = 0 to w-1 do
			s := !s ^ " " ^ string_of_float	mat.(i).(j)
		done;
		s := !s ^ "\n"
	done;
	Printf.printf "%s" !s

(* Pas de distribution gaussienne pour l'instant ... *)
let mutate m factor =
	map m (fun a -> a +. Random.float_range (-.factor) factor)