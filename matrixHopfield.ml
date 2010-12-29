(* Matrices symétrique à diagonale nulle *)

open Primitives

include Matrix

let random n n0 maxi =
	assert (n=n0);
	let m = Matrix.make_matrix n n 0. in
	for i = 1 to n-1 do
		for j = 0 to i-1 do
			let rand = Random.float maxi in
			m.(i).(j) <- rand;
			m.(j).(i) <- rand
		done
	done;
	m
	
let random_range n n0 mini maxi =
	assert (n=n0);
	let m = Matrix.make_matrix n n 0. in
	for i = 1 to n-1 do
		for j = 0 to i-1 do
			let rand = Random.float_range mini maxi in
			m.(i).(j) <- rand;
			m.(j).(i) <- rand
		done
	done;
	m

let mutate m factor =
	let new_m = Matrix.mutate m factor in
	for i = 0 to Array.length m - 1 do
		new_m.(i).(i) <- 0.
	done;
	new_m