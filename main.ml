open Dataset
open Event
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
	
let iterate (m:map) ~hop ~display =
	update m ~hop;
	remove_escaped m;
	if display then 
	(* Pause de 100ms *)
	(wait 100; redraw m )


let test_map ?(display=false) ?(max_time=200) map hop =
	let rec loop n =
	    if List.length map.people = 0 || n = 0 then
	        ()
	    else
	        (iterate map ~hop ~display:false; loop (n-1))
	in
	    let rec loop_display n polling =
	        if List.length map.people = 0 || n = 0 then
	            ()
    	    else match wait_next_event (if polling then [Poll;Key_pressed] else [Key_pressed]) with
    	        | status when status.keypressed ->
    	            (match parse_keypressed status.key with
    	                | Zoom dir -> (zoom_screen dir; iterate map ~display ~hop; loop_display (n-1) false)
    	                | Move dir -> (move_screen dir; iterate map ~display ~hop; loop_display (n-1) false)
    	                | NoDisplay -> (iterate map ~display ~hop; loop (n-1))
    	                | Quit -> ()
    	                | Nothing -> (iterate map ~display ~hop; loop_display (n-1) (not polling)))
    	        | _ -> 
    			    (iterate map ~display ~hop; loop_display (n-1) true)
	in
	    (if display then loop_display max_time true
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
	
let final_test ?(display=false) map neural_net =
    map.people <- map_range 5 5 (fun i j -> new person {x=2.+.4.*.float_of_int i; y=2.+.4.*.float_of_int j}  (Random.float_range 0. 1.));
    test_map map neural_net ~display ~max_time:300
	
let blank_test ?(display=false) map neural_net =
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

		}
	in
	
	let walls2 =
	    [
	        (* Murs extérieurs *)
	        fast_wall 0. 0. 50. 0.;
	        fast_wall 0. 0. 0. 50.;
	        fast_wall 0. 50. 50. 50.;
	        fast_wall 50. 0. 50. 50.;
	        (* Murs intérieurs *)
	        fast_wall 30. 0. 30. 20.;
	        fast_wall 30. 25. 30. 50.;
	        fast_wall 25. 21. 25. 24.
	    ]
	in
	let boxes2 = 
	    [
	        fast_box 0. 0. 30. 50. 35. 22.;
	        fast_box 30. 0. 50. 50. 45. 25.;
        ]
	in
	let final_map =
	    {
	        w = 50;
	        h = 50;
	        obstacles = walls2;
	        boxes = boxes2;
	        people = [];
	        final_exit = {x=45.; y=25.}
	    }

	in

	let hop = new Hopfield.t 12 4 1 Hopfield.step in
	hop#init;

	let pop1 = HopfieldEvoluate.generate_population hop 2. 500 in
	let best1 = HopfieldEvoluate.choose_best (fast_test_map my_map) pop1 in
    Printf.printf "%d\n" (fast_test_map my_map best1); flush stdout;
    
	let pop2 = HopfieldEvoluate.generate_population best1 2. 100 in
	let best2 = HopfieldEvoluate.choose_best (close_test_map my_map) pop2 in
    Printf.printf "%d\n" (fast_test_map my_map best2); flush stdout;
    
	let pop3 = HopfieldEvoluate.generate_population best2 1. 50 in
	let best3 = HopfieldEvoluate.choose_best (deep_test my_map) pop3 in
    Printf.printf "%d\n" (fast_test_map my_map best3); flush stdout;
    
	if fast_test_map my_map best3 = 0 then
		(*Printf.printf "%d\n" (deep_test ~display:true my_map best3)*)
		Printf.printf "%d\n" (deep_test ~display:true my_map best3)
	else
		Printf.printf "Désolé, je suis encore trop jeune pour toi.\n"