open Primitives


(**************************
 *  TYPES : POINT & WALL  *
 **************************)

type point =
    {
        x : float;
        y : float
    }
    
type wall =
    {
        p1 : point;
        p2 : point;
        wall_id : int
    }


let print_point p =
    Printf.printf "(%f %f)\n" p.x p.y


let print_wall w =
    Printf.printf "((%f %f), (%f %f))\n" w.p1.x w.p1.y w.p2.x w.p2.y



(* Operations on points and walls *)

let fast_wall a b c d = 
	{
	    p1 =
	        {
	            x = a;
	            y = b
	        };
	    p2 =
	        {
	            x = c;
	            y = d
	        };
	    wall_id = Primitives.unique_id ()
	}


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

class person (_p:point) (_t:float) _id =
	let r = 1. in
	let sensors_r = 2.5 in
	let sensors_gap = 1. in
	
	object (s)
		val mutable p = _p
		val mutable t = _t
		val id = if _id <> 0 then _id else Primitives.unique_id ()
		
		method get_id = id
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


type obstacle_t =
    | Wall of wall


let point_mbr = function
    | {x = x; y = y} ->
       ( x -. 1., x +. 1., y -. 1., y +. 1.:Mbr.t)


let wall_mbr = function
    | { p1 = p1; p2 = p2; wall_id = _ } ->
        min p1.x p2.x, max p1.x p2.x, min p1.y p2.y, max p1.y p2.y


module Person = struct

    type t = person

    let get_id p = p#get_id

    let get_mbr p =
        let p0 = p#get_point
        and r = p#radius in
        p0.x -. r, p0.x +. r, p0.y -. r, p0.y +. r
end


let fast_make_obstacle_list liste =
    List.map (fun w -> Wall w) liste


module Obstacle = struct
    
    type t = obstacle_t
    
    let get_id = function
        | Wall w -> w.wall_id
    
    let get_mbr = function
        | Wall w0 ->
            wall_mbr w0
    
end


module RtreeObstacle = Rtree.Rtree (Obstacle)


module RtreePeople = Rtree.Rtree (Person)



(*
    Le type map contient toutes les information sur une carte :
     - hauteur et largeur de la carte
     - obstacles et personnes présentes (dont la liste de leurs ids)
     - liste des boxes permettant une orientation rapide et de la sortie finale
*)

type map =
	{
		w : int;
		h : int;
		obstacles : RtreeObstacle.t;
		people : RtreePeople.t;
		id_list : int list;
		boxes : box list;
		final_exit : point
	}


(* Renvoie une nouvelle map dans laquelle une liste de personne a été insérée *)

let add_map_people map (people_list:Person.t list) =
    let id_liste = List.map (fun p -> p#get_id) people_list in
    {
        map with
            people = RtreePeople.insert_list people_list RtreePeople.empty;
            id_list = id_liste
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

(*
    Vérifie si deux obstacles s'intersectent. Un test préliminaire est effectué avec les mbrs
*)

let is_there_col_obstacle w1 w2 = match (w1, w2) with
    | Wall w1_0, Wall w2_0 ->
        if Mbr.intersect (Obstacle.get_mbr w1) (Obstacle.get_mbr w2) then
            is_there_col_walls w1_0 w2_0
        else
            false
    

(* Détermine si un segment intersecte les obstacles de la carte *)

let is_there_collision_wall m = function
    | Wall w0 as w ->
        (* Recherche des voisins du mur *)
        let possible_walls = RtreeObstacle.find_mbr (Obstacle.get_mbr w) m.obstacles in
        (* Existe-t-il un obstacle qui l'intersecte ? *)
        assume_once (is_there_col_obstacle w) (List.map snd possible_walls)


(* Teste si un point est à l'intérieur du disque que délimite une personne *)

let is_in_person p p0 =
	dist p p0#point < p0#radius


(* Est-ce qu'un point se serait caché dans quelqu'un ?? *)

let is_there_collision_point m p =
    (* Mbr rectangulaire ... *)
    let possible_people = RtreePeople.find_mbr (point_mbr p) m.people in
	assume_once (fun p0->is_in_person p p0) (List.map snd possible_people)


(* Détermine la liste des collisions pour les 3 senseurs d'une personne *)

let get_sensors_col someone m =
	Array.map
		(fun s ->
		    let pseudo_obstacle = Wall {p1=someone#point; p2=s; wall_id = 0} in
			is_there_collision_point m s || is_there_collision_wall m pseudo_obstacle
		)
		(someone#get_sensors)


(*
    Renvoie la distance minimal entre un point et une liste de murs. Si aucun
    mur n'a été passé en entrée (liste vide), on renvoie infinity.
*)

let min_dist_to_walls p =
	let rec aux = function
		| [] -> infinity (* heureusement que cette "constante" existe *)
		| (Wall hd) :: tl -> min (dist_to_wall p hd) (aux tl) in
		aux


(* Vérifie si une personne est bien positionnée *)

let is_there_col_people_id m p =

    let nearest_obstacles = (List.map snd (RtreeObstacle.find_mbr (Person.get_mbr p) m.obstacles)) in
    
    let p0 = p#point and r = p#radius in
    let box = (p0.x -. 2. *. r, p0.x +. 2. *. r, p0.y -. 2. *. r, p0.y +. 2. *. r) in
    let nearest_people = (List.map snd (RtreePeople.find_mbr box m.people)) in
    
    min_dist_to_walls p#point nearest_obstacles < p#radius ||
	assume_once
		(fun p0->
			if p0#get_id <> p#get_id then
				dist p0#point p#point <= p0#radius +. p#radius
			else false (*eh c'est moi !*)
		)
		nearest_people

(* Vérifie si une personne sera bien positionné après un déplacement donné *)

let is_there_future_col_people m p futurepos =
    let r = p#radius in
    
    let nearest_obstacles = List.map snd (RtreeObstacle.find_mbr (point_mbr futurepos) m.obstacles) in
    let box = (futurepos.x -. 2. *. r, futurepos.x +. 2. *. r, futurepos.y -. 2. *.r, futurepos.y +. 2. *. r) in
    let nearest_people = (List.map snd (RtreePeople.find_mbr box m.people)) in
    
	min_dist_to_walls futurepos nearest_obstacles < p#radius ||
	assume_once
		(fun p0 ->
			Person.get_id p0 <> Person.get_id p && dist p0#point futurepos <= p0#radius +. p#radius
		)
		nearest_people



(**
    Mise à jour de la carte par méthode itérative.
    Les personnes se déplacent les unes après les autres.
**)

let pas = 0.5
let angle_factor = 0.5


(* Mise à jour pour une seule personne *)

let update_person m id ~hop =
    let someone = RtreePeople.find_id id m.people in
    let p0 = someone#point in
    
    let new_0 = RtreePeople.remove_id id m.people in

    (* Entrée passée au réseau de neurone *)
    let hop_input =
    	let pos_out = get_exit m.boxes someone#point in
    	let dx = pos_out.x -. p0.x in
    	let dy = pos_out.y -. p0.y in
    	let future_angle = atan (dy/.dx) in
    	let dteta =  min_en_norme (future_angle -. someone#angle) (future_angle -. someone#angle -. 6.28) in
    	let f = function
    		| true -> 1.
    		| false -> -. 1. in
    	Array.append (Array.map f (get_sensors_col someone m)) [|sign_float dteta|] in
    
    (* Nouvel angle déterminé grace au réseau de neurones *)
    let t = (someone#angle +. (hop#get_rule hop_input).(0) *. angle_factor) in

    let future_pos =
        {x = p0.x +. pas *. cos t; y = p0.y +. pas *. sin t} in    
    
    (* On teste si la personne a trouvé la sortie *)
    if dist p0 (m.final_exit) < 5.*.someone#radius then
        {m with
            people = new_0;
            id_list = List.filter (fun i -> i <> id) m.id_list
        }
    else
        (
            (* Sinon on effectue, si possible, le déplacement *)
            let a =
                if not (is_there_future_col_people m someone future_pos) then
                    new person future_pos t someone#get_id
                else
                    new person p0 t someone#get_id in
            
            let new_rtree_people = RtreePeople.insert a new_0 in
            {m with people = new_rtree_people}
        )


(* Mise à jour de la carte *)

let update_map m ~hop =
    List.fold_left (fun map i -> update_person map i ~hop) m m.id_list
