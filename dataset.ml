open Primitives


(**************************
 *  TYPES : POINT & WALL  *
 **************************)

type point = {x : float; y : float}
type wall = {p1 : point; p2 : point}

(* Operations on points and walls *)

let fast_wall a b c d = 
	{p1 = {x = a; y = b}; p2 = {x = c; y = d}}

let add_vect p = function
	| {x=a;y=b} -> {x = p.x +. a; y = p.y +. b}

let make_vect r t =
	{x=r*.cos t; y=r*.sin t}
	
let dist p1 p2 =
	sqrt ((p1.x -. p2.x)**2. +. (p1.y -. p2.y)**2.)
	
(* Distance à un segment *)
(*         C             *)
(*       / |             *)
(*     /   |             *)
(*   /     |             *)
(* A-------H--------B    *)
(* Attention au cas ou le projeté "dépasse" ! *)
let dist_to_wall c w =
	let a, b = w.p1, w.p2 in
	let p, q = (b.x -. a.x) /. dist a b, (b.y -. a.y) /. dist a b in
	let s = p *. (c.x -. a.x) +. q *. (c.y -. a.y) in
	let p = {x = a.x +. s*.p; y = a.y +. s*.q} in
	if is_in_range p.x a.x b.x && is_in_range p.y a.y b.y then
		dist c p
	else min (dist c a) (dist c b)


(***************************
 *  TYPES : PERSON & WALL  *
 ***************************)

class person (_p:point) (_t:float) =
	let r = 1. in
	let sensors_r = 2. in
	let sensors_gap = 1.5 in
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

class box _p1 _p2 (_p_out:point) =
	object (s)
		val p1 = _p1
		val p2 = _p2
		val p_out = _p_out
		
		method p1=p1
		method p2=p2

		method is_in p =
			is_in_range p.x p1.x p2.x && is_in_range p.y p1.y p2.y

		method get_exit = p_out

	end

let get_exit box_list p =
	(List.find (fun b->b#is_in p) box_list)#get_exit
	

let fast_box x1 y1 x2 y2 xe ye = new box {x=x1;y=y1} {x=x2;y=y2} {x=xe;y=ye}

type map =
	{
		w : int;
		h : int;
		mutable obstacles : wall list;
		mutable people : person list;
		mutable boxes : box list;
		mutable final_exit : point
	}


(* Détermine si deux segments s'intersectent *)
let is_there_col_walls w1 w2 = 
	let x1,x2,x3,x4 = w1.p1.x, w1.p2.x, w2.p1.x, w2.p2.x
	and y1,y2,y3,y4 = w1.p1.y, w1.p2.y, w2.p1.y, w2.p2.y in

	let a = (x1-.x2)*.(y3-.y4) -. (y1-.y2)*.(x3-.x4) in
	let b = (x4-.x3)*.(y1-.y3) -. (y4-.y3)*.(x1-.x3) in
	
	let px = x1 +. b *. (x2-.x1) /. a
	and py = y1 +. b *. (y2-.y1) /. a in

	is_in_range px x1 x2 && is_in_range px x3 x4 &&
	is_in_range py y1 y2 && is_in_range py y3 y4

(* Détermine naivement si un segment intersecte les obstacles de la carte *)
let is_there_collision_wall w0 m =
	assume_once (is_there_col_walls w0) m.obstacles

let is_in_person p p0 =
	dist p p0#point < p0#radius

(* Est-ce qu'un point se serait caché dans quelqu'un ?? *)
let is_there_collision_point p m =
	assume_once (fun p0->is_in_person p p0) m.people


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
	min_dist_to_walls someone#point m.obstacles < someone#radius ||
	assume_once
		(fun p0->
			if p0#point <> someone#point then
				dist p0#point someone#point <= p0#radius +. someone#radius
			else false (*eh c'est moi !*)
		)
		m.people

let is_there_future_col_people someone m futurepos =
	min_dist_to_walls futurepos m.obstacles < someone#radius ||
	assume_once
		(fun p0 ->
			if p0#point <> someone#point then
				dist p0#point futurepos <= p0#radius +. someone#radius
			else false
		)
		m.people