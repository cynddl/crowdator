include Graphics

open Dataset
open Primitives



(* Facteur de grandissement *)
let ti f = int_of_float (10. *. f)

let draw_line p1 p2 =
	moveto (ti p1.x) (ti p1.y);
	lineto (ti p2.x) (ti p2.y)

(* Affiche un point à l'écran *)
let display_point p =
	draw_circle (ti p.x) (ti p.y) 1

(* Affiche un mur, i.e. un segment *)
let display_wall w =
	moveto (ti w.p1.x) (ti w.p1.y);
	lineto (ti w.p2.x) (ti w.p2.y)

(* Affiche une persone :
	- disque pour le corps
	- positions de chaque senseur, colorié en bleu s'il est activé
*)
let display_person (p0:person) map =
	let p = p0#get_point in
	draw_circle (ti p.x) (ti p.y) (ti p0#radius);

	(*Direction du bonhomme*)
	moveto (ti p.x) (ti p.y);
	lineto (ti (p.x +. p0#sensors_radius *. cos p0#angle))
	       (ti (p.y +. p0#sensors_radius *. sin p0#angle));

	Array.iter2
		(fun s b ->
			if b then set_color blue;
			display_point s;
			set_color black
		)
		p0#get_sensors
		(get_sensors_col p0 map)

let display_box b =
	draw_rect (ti(min b#p1.x b#p2.x)) (ti(min b#p1.y b#p2.y)) (ti(abs_float (b#p2.x -. b#p1.x)))  (ti(abs_float (b#p2.y -. b#p1.y)));
	display_point b#get_exit

(* Affiche chaque personne et chaque mur d'une map *)
let display_map map =
	set_color green;	List.iter display_box map#boxes; set_color black;
	List.iter display_wall map#obstacles;
	List.iter (fun s -> display_person s map) map#people


let start_display =
	open_graph "";
	moveto 10 10; draw_string "101010 !"
	
let redraw m =
	clear_graph (); display_map m