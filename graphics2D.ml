include Graphics


open Dataset
open Event
open Primitives
open Mbr
open Rtree


(**
    Le monde où se déplacent les personnes est dans un système de coordonnées propres,
    qu'il faut transformer pour afficher à l'écran. On utilise pour cela deux types de
    coordonnées :
     - coordonnées virtuelles : celles des personnages ;
     - coordonnées réelles : celles de l'écran.
**)

type screen_params =
	{
	    mutable center : int * int;
		mutable zoom : float;
	}


let screen =
	{ center = (0,0); zoom = 10.}
	
(** Fonctions de conversion des coordonnées **)

let virtual_to_real (x,y) =
    let cx, cy = screen.center in
    (cx + int_of_float (screen.zoom *. (x -. float cx)),
     cy + int_of_float (screen.zoom *. (y -. float cy)))


(* Homothétie d'affichage*)
let ti f =
	int_of_float (screen.zoom *. f)

(* Transforme un point en couple d'entiers mis à l'échelle *)
let hm p = 
    virtual_to_real (p.x, p.y)


(* Fonctions d'affichage *)	

let draw_line p1 p2 =
	let _p1, _p2 = hm p1, hm p2 in
	moveto (fst _p1) (snd _p1);
	lineto (fst _p2) (snd _p2)

(* Affiche un point à l'écran *)
let display_point p =
	let _p = hm p in
	draw_circle (fst _p) (snd _p) 1
	
let display_circle p r =
	let _p = hm p in
	draw_circle (fst _p) (snd _p) (ti r)

(* Affiche un mur, i.e. un segment *)
let display_wall w =
	draw_line w.p1 w.p2
	
let display_rect p1 p2 =
	let x1, x2 = min (fst (hm p1)) (fst (hm p2)), max (fst (hm p1)) (fst (hm p2))
	and y1, y2 = min (snd (hm p1)) (snd (hm p2)), max (snd (hm p1)) (snd (hm p2)) in
	draw_rect x1 y1 (x2-x1) (y2-y1)


(**
    Affiche une persone :
	 - disque pour le corps
	 - positions de chaque senseur, colorié en bleu s'il est activé
**)
let display_person (p0:person) map =
	let p = p0#get_point in
	display_circle p p0#radius;

	(*Direction du bonhomme*)
	draw_line p (add_vect p (make_vect p0#sensors_radius p0#angle))(*;*)

	(* Affichage des senseurs *)
	(*Array.iter2
		(fun s b ->
			if b then set_color blue;
			display_point s;Ò
			set_color black
		)
		p0#get_sensors
		(get_sensors_col p0 map)*)



(** MBR & R-tree displaying **)

let display_mbr (x0, x1, y0, y1) =
    display_rect {x=x0; y=y0} {x=x1; y=y1}

let display_obstacles map =
    let aux_display = function
        | Wall w ->
            display_wall w
    in
    RtreeObstacle.iter aux_display map.obstacles

let rec display_people map =
    let aux_display p =
            display_person p map
    in
    RtreePeople.iter(*_all (fun m -> set_color blue;display_mbr m; set_color black)*) aux_display map.people



let display_box b =
	display_rect b#p1 b#p2;
	display_point b#get_exit

(* Affiche chaque personne et chaque mur d'une map *)
let display_map map =
	set_color green;
	List.iter display_box map.boxes;
	set_color black;
	display_obstacles map;
	display_people map;
	synchronize ()


(** Déplacement de la zone d'affichage par l'utilisateur **)

let move_screen dir =
    let cx, cy = screen.center in
    let (dx, dy) = match dir with
        | Up -> (0, 2)
        | Down -> (0, -2)
        | Left -> (-2, 0)
        | Right -> (2, 0)
    in
        screen.center <- (cx+dx, cy+dy)

let zoom_screen dir =
    let r = match dir with
        | In -> 1.
        | Out -> -1.
    in
        screen.zoom <- screen.zoom +. r

let start_display =
	open_graph "";
	(* Pour éviter le clignotement *)
	auto_synchronize false

let redraw m ?(debug=false)=
	if not debug then clear_graph (); display_map m