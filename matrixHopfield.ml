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
	let n = Array.length m in
	let new_m = Matrix.make_matrix n n 0. in
	for i = 1 to n-1 do
		for j = 0 to i-1 do
			let rand = m.(i).(j) +. Random.float_range (-.factor) factor in
			new_m.(i).(j) <- rand;
			new_m.(j).(i) <- rand
		done
	done;
	new_m