open Dataset
open Evolution
open Hopfield
open Graphics2D
open Primitives


(* Déplacement à l'aide d'un réseau de Hopfield *)

let pas = 0.5
let angle_factor = 0.5

let hop_input someone m =
	let pos_out = get_exit m.boxes someone#point in
	let dx = pos_out.x -. someone#point.x in
	let dy = pos_out.y -. someone#point.y in
	let future_angle = atan (dy/.dx) in
	let dteta =  min_en_norme (future_angle -. someone#angle) (future_angle -. someone#angle -. 6.28) in
	let f = function
		| true -> 1.
		| false -> -. 1. in
	Array.append (Array.map f (get_sensors_col someone m)) [|sign_float dteta|]


let update_angle someone m ~hop =
	let arr = hop_input someone m in
	someone#set_angle (someone#angle +. (hop#get_rule arr).(0) *. angle_factor)

let update_one someone m ~hop =
	let p = someone#point in
	update_angle someone m ~hop;
	let future_pos = {x=p.x +. pas *. cos someone#angle; y=p.y +. pas *. sin someone#angle} in
	if not (is_there_future_col_people someone m future_pos) then
		someone#set_point future_pos

let update m ~hop =
	List.iter (fun p -> update_one p m ~hop) m.people

let remove_escaped m =
	m.people <- (List.filter (fun p0->dist p0#point (m.final_exit) > 5.*.p0#radius) m.people)
	
let iterate (m:map) ~display ~hop =
	update m ~hop;
	remove_escaped m;
	if display then 
	(* Pause de 100ms *)
	(Primitives.wait 100; redraw m )


let test_map ?(display=false) ?(max_time=200) map hop =
	let rec loop n =
	    if List.length map.people = 0 || n = 0 then
	        ()
	    else
	        (iterate map ~display ~hop; loop (n-1))
	in
	    let rec loop_display n =
	        if List.length map.people = 0 then ()
    	    else match (n, key_pressed ()) with
    		    | (_, true)
    		    | (0, _) ->
    			    ()
    		    | (_ , false) ->
    			    (iterate map ~display ~hop; loop_display (n-1))
	in
	    (if display then loop_display max_time
        else loop max_time);
		List.length map.people

let fast_test_map map neural_net =
	map.people <-
		[ (new person {x=10.; y=18.}  (Random.float_range 0. 1.));
		  (new person {x=15.; y=13.}  (Random.float_range 0. 1.));
		  (new person {x=10.; y=10.}  (Random.float_range 0. 1.)) ];
	test_map map neural_net
	
let close_test_map map neural_net =
    map.people <-
		[ (new person {x=10.; y=15.}  (Random.float_range 0. 1.));
		  (new person {x=15.; y=13.}  (Random.float_range 0. 1.));
		  (new person {x=10.; y=10.}  (Random.float_range 0. 1.)) ];
	test_map map neural_net
	
	
let deep_test ?(display=false) map neural_net =
	map.people <- map_range 3 7 (fun i j -> new person {x=5.+.4.*.float_of_int i; y=5.+.4.*.float_of_int j}  (Random.float_range 0. 1.));
	test_map map neural_net ~display ~max_time:300

let _ =
	Random.self_init ();
	
	let walls =
		[
			(fast_wall 2. 42. 42. 42.);
			(fast_wall 2. 2. 2. 42.);
			(fast_wall 2. 2. 42. 2.);
			(fast_wall 42. 2. 42. 42.);
			(fast_wall 30. 2. 30. 25.);
			(fast_wall 30. 30. 30. 42.);
			(fast_wall 20. 20. 20. 42.)
		] in
	let boxes = 
		[
			(fast_box 2. 2. 20. 42. 25. 10.);
		    (fast_box 20. 2. 30. 42. 32. 28.);
		    (fast_box 30. 2. 42. 42. 38. 10.)
		] in
	let my_map = 
		{
			w = 42;
			h = 42;
			obstacles = walls;
			people = [];
			boxes = boxes;
			final_exit = {x=40.; y=12.}

		} in
	let hop = new Hopfield.t 12 4 1 Hopfield.step in
	hop#init;

	let pop1 = HopfieldEvoluate.generate_population hop 3. 100 in
	let best1 = HopfieldEvoluate.choose_best (close_test_map my_map) pop1 in

	let pop2 = HopfieldEvoluate.generate_population best1 1. 100 in
	let best2 = HopfieldEvoluate.choose_best (fast_test_map my_map) pop2 in

	let pop3 = HopfieldEvoluate.generate_population best2 1. 100 in
	let best3 = HopfieldEvoluate.choose_best (fast_test_map my_map) pop3 in

	if fast_test_map my_map best3 = 0 then
		Printf.printf "%d\n" (deep_test ~display:true my_map best3)
	else
		Printf.printf "Désolé, je suis encore top jeune pour toi.\n"