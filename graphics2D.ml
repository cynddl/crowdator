open Graphics
open Primitives
	
type point = {x:float; y:float}
type wall = {p1:point; p2:point}



(*~ Primitives générales ~*)

let fast_wall a b c d = 
	{p1 = {x = a; y = b}; p2 = {x = c; y = d}}

let add_vect p = function
	| (a,b) -> {x = p.x +. a; y = p.y +. b}

let make_vect r t =
	(r*.cos t, r*.sin t)

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
	let a,b = w.p1,w.p2 in
	let ah = abs_float ((b.x-.a.x)*.(c.x-.a.x) +. (b.y-.a.y)*.(c.y-.a.y)) /. (dist a b) in
	min3 (sqrt ((dist a c)**2.-. ah**2.)) (dist a c) (dist a b)



(*~ Primitives d'affichage ~*)

(* Facteur de grandissement *)
let ti f = int_of_float (10. *. f)

let draw_line p1 p2 =
	moveto (ti p1.x) (ti p1.y);
	lineto (ti p2.x) (ti p2.y)

(* Affiche un point à l'écran *)
let display_point p =
	draw_circle (ti p.x) (ti p.y) 1



