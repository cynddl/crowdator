open Dataset
open Evolution
open Hopfield
open Graphics2D
open Primitives


(* Déplacement à l'aide d'un réseau de Hopfield *)

let pas = 0.5
let angle_factor = 0.5

let hop_input someone m =
	let pos_out = m#get_exit someone#point in
	let dx = pos_out.x -. someone#point.x in
	let dy = pos_out.y -. someone#point.y in
	let future_angle = atan (dy/.dx) in
	let dteta =  min_en_norme (future_angle -. someone#angle) (future_angle -. someone#angle -. 6.28) in
	let f = function
		| true -> 1.
		| false -> -. 1. in
	Array.append (Array.map f (get_sensors_col someone m)) [|dteta/.abs_float dteta|]


let update_angle someone m ~hop =
	let arr = hop_input someone m in
	if (hop#get_rule arr).(0) = 1. then
		someone#set_angle (someone#angle +. (hop#get_rule arr).(1) *. angle_factor)
	else ()

let update_one someone m ~hop =
	let p = someone#point in
	update_angle someone m ~hop;
	let future_pos = {x=p.x +. pas *. cos someone#angle; y=p.y +. pas *. sin someone#angle} in
	if not (is_there_future_col_people someone m future_pos) then
		someone#set_point future_pos

let update m ~hop =
	List.iter (fun p -> update_one p m ~hop) m#people

let remove_escaped m =
	m#set_people (List.filter (fun p0->dist p0#point (m#get_final_exit) > 5.*.p0#radius) m#people)
	
let iterate (m:map) ~display ~hop =
	update m ~hop;
	remove_escaped m;
	if display then 
	(* Pause de 100ms *)
	(Primitives.wait 100; redraw m )



let test_hop ~display m hop =
	let rec loop n = match (n, key_pressed () || List.length m#people = 0) with
		| (_, true)
		| (0, _) ->
			()
		| (_ , false) ->
			(iterate m ~display ~hop; loop (n-1))
	in
	m#clean;
	m#add_person (new person {x=10.; y=18.}  (Random.float_range 0. 1.));
	m#add_person (new person {x=15.; y=13.}  (Random.float_range 0. 1.));
	m#add_person (new person {x=10.; y=10.}  (Random.float_range 0. 1.));
	(*for i = 0 to 3 do
		for j = 0 to 7 do
			m#add_person (new person {x=5.+.4.*.float_of_int i; y=5.+.4.*.float_of_int j}  (Random.float_range 0. 1.))
		done
	done;*)
	loop 200;
	List.length m#people

let test_hop_final ~display m hop =
	let rec loop n = match (n, key_pressed () || List.length m#people = 0) with
		| (_, true)
		| (0, _) ->
			()
		| (_ , false) ->
			(Printf.printf "%d %d\n" n (List.length m#people); iterate m ~display ~hop; loop (n-1))
	in
	m#clean;
	for i = 0 to 3 do
		for j = 0 to 7 do
			m#add_person (new person {x=5.+.4.*.float_of_int i; y=5.+.4.*.float_of_int j}  (Random.float_range 0. 1.))
		done
	done;
	loop 300;
	List.length m#people


let _ =
	Random.self_init ();
	
	let my_map = new map {x=42.; y=10.} in
	my_map#add_wall (fast_wall 2. 42. 42. 42.);
	my_map#add_wall (fast_wall 2. 2. 2. 42.);
	my_map#add_wall (fast_wall 2. 2. 42. 2.);
	my_map#add_wall (fast_wall 42. 2. 42. 42.);
	
	my_map#add_wall (fast_wall 30. 2. 30. 25.);
	my_map#add_wall (fast_wall 30. 30. 30. 42.);
	
	my_map#add_wall (fast_wall 20. 20. 20. 42.);
	
	my_map#add_box (fast_box 2. 2. 20. 42. 25. 10.);
	my_map#add_box (fast_box 20. 2. 30. 42. 32. 28.);
	my_map#add_box (fast_box 30. 2. 42. 42. 38. 10.);
	
	start_display;
	let hop = new Hopfield.t 12 4 2 Hopfield.step in
	hop#init;

	let pop1 = HopfieldEvoluate.generate_population hop 3. 100 in
	let best1 = HopfieldEvoluate.choose_best (test_hop ~display:false my_map) pop1 in
	
	let pop2 = HopfieldEvoluate.generate_population best1 1. 100 in
	let best2 = HopfieldEvoluate.choose_best (test_hop ~display:false my_map) pop2 in

	let pop3 = HopfieldEvoluate.generate_population best2 1. 100 in
	let best3 = HopfieldEvoluate.choose_best (test_hop ~display:false my_map) pop3 in

	if test_hop ~display:false my_map best3 = 0 then
	Printf.printf "%d\n" (test_hop_final ~display:true my_map best3)
	else Printf.printf "Désolé, je suis encore top jeune pour toi."

