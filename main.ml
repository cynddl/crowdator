open Dataset
open Event
open Evolution
open Hopfield
open Graphics2D
open Primitives


let iterate (m:map) ~hop ~display ~debug=
	let m1 = update_map m ~hop in
	if display then 
	    (* Pause de 100 ms *)
	    (wait 100; redraw ~debug m1);
	m1



let test_map ?(display=false) ?(debug=false) ?(max_time=200) map hop =
	let rec loop m n =
	    if RtreePeople.size m.people = 0 || n = 0 then
	        m
	    else
	        (let m0 = update_map m ~hop in loop m0 (n-1))
	in
	    let rec loop_display m n polling =
	        if RtreePeople.size m.people = 0 || n = 0 then
	            m
    	    else match wait_next_event (if polling then [Poll;Key_pressed] else [Key_pressed]) with
    	        | status when status.keypressed ->
    	            (match parse_keypressed status.key with
    	                | Zoom dir ->
    	                    (zoom_screen dir; let m0 = iterate m ~display ~debug ~hop in loop_display m0 (n-1) false)
    	                | Move dir ->
    	                    (move_screen dir; let m0 = iterate m ~display ~debug ~hop in loop_display m0 (n-1) false)
    	                | NoDisplay ->
    	                    (let m0 = iterate m ~display ~debug ~hop in loop m0 (n-1))
    	                | Quit ->
    	                    m
    	                | Nothing ->
    	                    (let m0 = iterate m ~display ~debug ~hop in loop_display m0 (n-1) (not polling)))
    	        | _ -> 
    			    (let m0 = iterate m ~display ~debug
    			         ~hop in loop_display m0 (n-1) true)
	in
	    let last_map =
	        (
	            if display then
	                loop_display map max_time true
                else
                    loop map max_time
            )
        in
        RtreePeople.size last_map.people



let fast_test_map ?(display=false) map neural_net =
	let people_list =
        [(new person {x=10.; y=18.}  (Random.float_range 0. 1.) 0)]
    in
    let m = add_map_people map people_list in
    test_map ~display m neural_net


let close_test_map ?(display=false) map neural_net =
    let people_list =
        [(new person {x=10.; y=14.}  (Random.float_range 0. 1.) 0);
         (new person {x=13.; y=12.}  (Random.float_range 0. 1.) 0);
         (new person {x=10.; y=10.}  (Random.float_range 0. 1.) 0)]
    in
    let m = add_map_people map people_list in
    test_map ~display m neural_net


let deep_test ?(display=false) map neural_net =
    let people_list =
        map_range 3 7 (fun i j -> new person {x=5.+.4.*.float_of_int i; y=5.+.4.*.float_of_int j}  (Random.float_range 0. 1.) 0) in
    let m = add_map_people map people_list in
	test_map m neural_net ~display ~max_time:300

	
let final_test ?(display=false) map neural_net =
    let people_list =
        map_range 5 5 (fun i j -> new person {x=2.+.4.*.float_of_int i; y=2.+.4.*.float_of_int j}  (Random.float_range 0. 1.) 0) in
    let m = add_map_people map people_list in
    test_map m neural_net ~display ~max_time:300

	
let blank_test ?(display=false) map neural_net =
    test_map map neural_net ~display ~max_time:300

let mini_test ?(display=false) ?(debug=false) map neural_net =
    let people_list =
        [(new person {x=10.; y=6.}  (Random.float_range 0. 1.) 0)]
    in
    let m = add_map_people map people_list in
    test_map ~display ~debug m neural_net



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
		    (*(fast_box 20. 2. 30. 25. 25. 28.);
		    (fast_box 20. 25. 30. 42. 32. 28.);*)
		    (fast_box 20. 2. 30. 42. 32. 28.);
		    (fast_box 30. 2. 42. 42. 38. 10.)
		] in
	let my_map = 
		{
			w = 42;
			h = 42;
			obstacles = RtreeObstacle.insert_list (fast_make_obstacle_list walls) RtreeObstacle.empty;
			people = RtreePeople.Empty;
			id_list = [];
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
	        fast_wall 30. 25. 30. 50.(*;
	        fast_wall 25. 21. 25. 24.*)
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
	        obstacles = RtreeObstacle.insert_list (fast_make_obstacle_list walls2) RtreeObstacle.Empty;
	        boxes = boxes2;
	        people = RtreePeople.Empty;
	        id_list = [];
	        final_exit = {x=45.; y=25.}
	    }
	in
	
	let mini_walls =
	    [
	        fast_wall 0. 0. 50. 0.;
	        fast_wall 0. 0. 0. 50.;
	        fast_wall 0. 50. 50. 50.;
	        fast_wall 50. 0. 50. 50.;
	        fast_wall 20. 15. 20. 35.;
	        fast_wall 25. 15. 25. 35.;
	        fast_wall 20. 15. 25. 15.;
	        fast_wall 20. 35. 25. 35.;
	        
	        fast_wall 30. 10. 30. 30.;
	        fast_wall 35. 10. 35. 30.;
	        fast_wall 30. 10. 35. 10.;
	        fast_wall 30. 30. 35. 30.
	    ]
	in
		
	let mini_map =
	    {
	        w = 50;
	        h = 50;
	        obstacles = RtreeObstacle.insert_list (fast_make_obstacle_list mini_walls) RtreeObstacle.Empty;
	        boxes = [fast_box 0. 0. 50. 50. 45. 25.];
	        people = RtreePeople.Empty;
	        id_list = [];
	        final_exit = {x=45.; y=25.}
	    }
	
	

	in

	let hop = new Hopfield.t 12 4 1 Hopfield.step in
	hop#init;

    let best0 = HopfieldEvoluate.elect_one hop 2. 1000 (fast_test_map my_map) in
    Printf.printf "Passe 0 : %d\n" (fast_test_map  my_map best0); flush stdout;

    let best1 = HopfieldEvoluate.elect_one best0 1. 100 (close_test_map my_map) in
    Printf.printf "Passe 1 : %d\n" (close_test_map my_map best1); flush stdout;

    let best2 = HopfieldEvoluate.elect_one best1 1. 100 (close_test_map my_map) in
    Printf.printf "Passe 2 : %d\n" (close_test_map my_map best2); flush stdout;
    
    let best3 = HopfieldEvoluate.elect_one best2 1. 1000 (close_test_map my_map) in
    Printf.printf "Passe 3 : %d\n" (close_test_map my_map best3); flush stdout;
    
    let best4 = HopfieldEvoluate.elect_one best3 1. 1000 (mini_test mini_map) in
    Printf.printf "Passe 4 : %d\n" (mini_test mini_map best4); flush stdout;

    let best5 = HopfieldEvoluate.choose_best (mini_test mini_map)
        [best0; best1; best2; best3; best4] in
    
    
    let best6 = HopfieldEvoluate.elect_one best5 1. 100 (deep_test my_map) in
    Printf.printf "Passe 5 : %d\n" (deep_test my_map best6); flush stdout;
    Printf.printf "Passe 5 : %d\n" (close_test_map my_map best6); flush stdout;
    
    (*Printf.printf "%d\n" (mini_test ~display:true ~debug:true mini_map best5);
    wait 10000;Unix.sleep 10*)
    
	if close_test_map my_map best6 = 0 then
	    (
	        Printf.printf "%d\n" (deep_test my_map best6);
	        Printf.printf "%d\n" (final_test final_map best6)
		)