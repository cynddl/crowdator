open Primitives
open Graphics2D
open Graphics

open Hopfield


module Array = struct
	include Array
	
	let iter2 f a1 a2 =
  		if Array.length a1 <> Array.length a2
  			then raise (Invalid_argument "Array.iter2");
  		for i = 0 to Array.length a1 - 1 do
    		f a1.(i) a2.(i);
  		done
end


(*** Données : personnes et cartes ***)
class person (_p:point) (_t:float) =
	let r = 1. in
	let sensors_r = 3. in
	let sensors_gap = 0.7 in
	object (s)
		val mutable p = _p
		val mutable t = _t
		
		method get_point = p
		method angle = t
		method set_angle t0 = t<-t0
		method radius = r
		method sensors_radius = sensors_r
		method sensors_gap = sensors_gap

		(* Renvoie la liste des positions des 3 senseurs *)
		method get_sensors =
			let s1 = add_vect p (make_vect sensors_r t) in
			let s2 = add_vect p (make_vect sensors_r (t+.sensors_gap)) in
			let s3 = add_vect p (make_vect sensors_r (t-.sensors_gap)) in
			[|s1;s2;s3|]
			
		method point = p
		method set_point p0 = p<-p0
			
	end

class map =
	object (s)
		val mutable _people = []
		val mutable _obst = []
		
		method obstacles = _obst
		method people = _people
		method set_people l = _people <- l
		
		method add_person (someone:person) =
			_people <- someone::_people
		
		method add_wall (w:wall) =
			_obst <- w::_obst
	end


(* Détermine si deux segments s'intersectent *)
let is_there_col_walls w1 w2 = 
	let x1,x2,x3,x4 = w1.p1.x, w1.p2.x, w2.p1.x, w2.p2.x
	and y1,y2,y3,y4 = w1.p1.y, w1.p2.y, w2.p1.y, w2.p2.y in
	
	let px =  ((x1*.y2-.y1*.x2)*.(x3-.x4) -. (x1-.x2)*.(x3*.y4-.y3*.x4))
		   /. ((x1-.x2)*.(y3-.y4) -. (y1-.y2)*.(x3-.x4))
	and py =  ((x1*.y2-.y1*.x2)*.(y3-.y4) -. (y1-.y2)*.(x3*.y4-.y3*.x4))
	       /.  ((x1-.x2)*.(y3-.y4) -. (y1-.y2)*.(x3-.x4)) in
	(*draw_circle (ti px) (ti py) 1;*)
	(*draw_line w1.p1 w1.p2;
	draw_line w2.p1 w2.p2;*)
	is_in_range px x1 x2 && is_in_range px x3 x4 &&
	is_in_range py y1 y2 && is_in_range py y3 y4

(* Détermine naivement si un segment intersection les obstacles de la carte *)
let is_there_collision_wall w0 m =
	assume_once (is_there_col_walls w0) m#obstacles
	
let is_in_person p p0 =
	dist p p0#point <= p0#radius
	
(* Est-ce qu'un pint se serait caché dans quelqu'un ?? *)
let is_there_collision_point p m =
	assume_once (fun p0->is_in_person p p0) m#people

(* Détermine la liste des collisions pour les 3 senseurs*)
let get_sensors_col someone m =
	Array.map
		(fun s ->
			is_there_collision_point s m || is_there_collision_wall {p1=someone#point; p2=s} m
		)
		(someone#get_sensors)
		

(* Distance minimal entre un point et une liste de murs *)
let min_dist_to_walls p =
	let rec aux = function
		| [] -> infinity (* heureusement que cette "constante" existe *)
		| hd :: tl -> min (dist_to_wall p hd) (aux tl) in
		aux


(* Vérifie si une personne est bien positionnée *)
let is_there_col_people someone m =
	min_dist_to_walls someone#point m#obstacles < someone#radius ||
	assume_once
		(fun p0->
			if p0#point <>someone#point then
				dist p0#point someone#point <= p0#radius +. someone#radius
			else false (*eh c'est moi !*)
		)
		m#people


(* Déplacement à l'aide d'un réseau de Hopfield *)

let hop = new Hopfield.hopfield 10 3 2 Hopfield.norm;;
Random.self_init ();;
hop#init;;
let rule = hop#get_rule;;
Matrix.print [|(rule [| -.1.;-.1.;-.1. |])|];;
Matrix.print [|(rule [| 1.;-.1.;-.1. |])|];;
Matrix.print [|(rule [| -.1.;-.1.;1. |])|];;



let update_angle someone m =
	let f = function
		| true -> 1.
		| false -> -.1. in
	let arr = Array.map f (get_sensors_col someone m) in
	if (rule arr).(0) = 1. then
		someone#set_angle ((someone#angle +. (rule arr).(1))/.5.)
	else ()


	
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

(* Affiche un mur, i.e. un segment *)
let display_wall w =
	moveto (ti w.p1.x) (ti w.p1.y);
	lineto (ti w.p2.x) (ti w.p2.y)

(* Affiche chaque personne et chaque mur d'une map *)
let display_map map =
	List.iter display_wall map#obstacles;
	List.iter (fun s -> display_person s map) map#people

(* Initialisation du système *)
let start_display =
	open_graph "";
	moveto 10 10; draw_string "Welcome !"

let pas = 1.

let update_one someone m =
	let p = someone#point in
	let future_pos = {x=p.x +. pas *. cos someone#angle; y=p.y +. pas *. sin someone#angle} in
	if not (is_there_col_people someone m) then
		someone#set_point future_pos;
	update_angle someone m

let update m =
	List.iter (fun p->update_one p m) m#people
	
let iterate m =
	update m;
	Unix.sleep 1;
	clear_graph ();
	display_map m
	(*Unix.sleep 1*)

let randidi a b = a +. (b -. a) *. (Random.float 1.)

let _ =
	
	let my_map = new map in
	for i = 0 to 12 do
		let a = new person {x=randidi 5. 35.; y=randidi 5. 35.}  (randidi 0. 6.) in
		my_map#add_person a
	done;
	
	(*let first_boy = new person {x=5.; y=5.} 2. in*)
	(*let second_boy = new person {x=5.; y=12.} 2. in*)
	my_map#add_wall (fast_wall 2. 42. 42. 42.);
	my_map#add_wall (fast_wall 2. 2. 2. 42.);
	my_map#add_wall (fast_wall 2. 2. 42. 2.);
	my_map#add_wall (fast_wall 42. 2. 42. 42.);
	(*my_map#add_person first_boy;*)
	(*my_map#add_person second_boy;*)
	
	start_display;
	display_map my_map;
	
	let rec loop () = match key_pressed () with
		| true -> ()
		| false ->
			(iterate my_map; loop ())
	in loop ()