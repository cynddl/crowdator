open Dataset
open Evolution
open Hopfield
open Graphics2D
open Primitives


(* Déplacement à l'aide d'un réseau de Hopfield *)

let pos_out = {x=42.; y = 15.};;

Random.self_init ();;

let hop_input someone m =
	let dx = pos_out.x -. someone#point.x in
	let dy = pos_out.y -. someone#point.y in
	let future_angle = atan (dy/.dx) in
	let dteta = future_angle-.someone#angle /. abs_float (future_angle-.someone#angle) in
	let f = function
		| true -> 1.
		| false -> -. 1. in
	Array.append (Array.map f (get_sensors_col someone m)) [|dteta|]


let update_angle someone m ~hop =
	let arr = hop_input someone m in
	if (hop#get_rule arr).(0) = 1. then
		someone#set_angle (someone#angle +. (hop#get_rule arr).(1) /. 3.)
	else ()

let pas = 1.

let update_one someone m ~hop =
	let p = someone#point in
	update_angle someone m ~hop;
	let future_pos = {x=p.x +. pas *. cos someone#angle; y=p.y +. pas *. sin someone#angle} in
	if not (is_there_future_col_people someone m future_pos) then
		someone#set_point future_pos

let update m ~hop =
	List.iter (fun p -> update_one p m ~hop) m#people

let remove_escaped m =
	m#set_people (List.filter (fun p0->dist p0#point pos_out > 10.*.p0#radius) m#people)
	
let iterate m ~display ~hop =
	update m ~hop;
	remove_escaped m;
	if display then 
	(Unix.sleep 1; redraw m )

let test_hop ~display hop =
	
	let my_map = new map in
	for i = 1 to 5 do
		let a = new person {x=15.; y=10. +. float_of_int i *. 4.}  (Random.float_range 0. 6.) in
		my_map#add_person a
	done;
	
	my_map#add_wall (fast_wall 2. 42. 42. 42.);
	my_map#add_wall (fast_wall 2. 2. 2. 42.);
	my_map#add_wall (fast_wall 2. 2. 42. 2.);
	my_map#add_wall (fast_wall 42. 2. 42. 42.);
	my_map#add_wall (fast_wall 30. 2. 30. 11.);
	my_map#add_wall (fast_wall 30. 31. 30. 42.);
	
	(* Au plus 100 itérations *)
	let rec loop n = match (n, key_pressed () || List.length my_map#people = 0) with
		| (_, true)
		| (0, _) -> ()
		| (_ , false) ->
			(iterate my_map ~display ~hop; loop (n-1))
	in loop 100;
	List.length my_map#people


let _ = 
	start_display;
	let hop = new Hopfield.t 10 4 2 Hopfield.step in
	hop#init;

	let pop1 = HopfieldEvoluate.generate_population hop 3. 100 in
	let best1 = HopfieldEvoluate.choose_best (test_hop ~display:false) pop1 in

	let pop2 = HopfieldEvoluate.generate_population best1 1. 100 in
	let best2 = HopfieldEvoluate.choose_best (test_hop ~display:false) pop2 in

	let pop3 = HopfieldEvoluate.generate_population best2 1. 100 in
	let best3 = HopfieldEvoluate.choose_best (test_hop ~display:false) pop3 in

	Printf.printf "%d\n" (test_hop ~display:true best3)

