open Matrix
open Mesh

let norm x =
	if x < 0. then -1.
	else 1.;;

class hopfield _w _in _out _func =
object (s)
	val mutable mesh = Mesh.new_mesh _w _func
	val w = _w
	val n_in = _in
	val n_out = _out
	val activation_func = _func
	
	(*		assert (n_in <= w && n_out <= w)	*)

	method init = mesh#randomise (-. 1.) 1.
	method get_mesh = mesh
	method set_mesh m = mesh <- m
	method get_w = w
	method get_n_in = n_in
	method get_n_out = n_out
	method get_activation_func = activation_func

	(* Applique un stimuli sur le réseau *)
	method present input =
		mesh#process input;
		mesh#get_state

	method get_rule =
		(fun vec_in ->
			assert (Array.length vec_in = n_in);
			let out = s#present (Matrix.make_unicol vec_in) in
			Array.init n_out (fun i-> out.(n_in+i).(0))
		)

end

let copy h =
	let hopi = new hopfield h#get_w h#get_n_in h#get_n_out h#get_activation_func in
	hopi#set_mesh (Mesh.copy h#get_mesh);
	hopi