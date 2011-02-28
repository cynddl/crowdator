(**
    Ce module permet de gérer les enveloppes ou MBR (Minimum Bounding Rectangle )
    nécessaires au fonctionnement des R-tree. Ce sont des rectangles "minimaux"
    entourant un objet donné.
**)

(* Coordonnées x puis y *)
type t = float * float * float * float

let empty = 0., 0., 0., 0.


(* Ajoute deux MBR *)
let add (x0, x1, y0, y1) (x0', x1', y0', y1') =
    (min x0 x0'), (max x1 x1'), (min y0 y0'), (max y1 y1')


(* Ajoute une liste de MBR *)
let add_list l =
    let rec aux acc =function
        | [] -> acc
        | hd :: tl -> aux (add hd acc) tl
    in
        match l with
            | [] -> raise (Invalid_argument "Empty list from Mbr.add_list")
            | hd :: tl -> aux hd tl


(* Teste si les deux rectangles se recoupent *)
let intersect (x0, x1, y0, y1) (x0', x1', y0', y1') =
    (* Teste si deux segments se recoupent *)
    let intersect_range r1 r2 r1' r2' =
        r1' <= r2 && r1 <= r2'
    in
        intersect_range x0 x1 x0' x1' && intersect_range y0 y1 y0' y1'


(* Aire d'un rectangle *)
let area (x0, x1, y0, y1) = (x1 -. x0) *. (y1 -. y0)


(* Teste si le premier rectangle est inclus dans le second *)
let is_in (x0, x1, y0, y1) (x0', x1', y0', y1') =
    x0 >= x0' && x1 <= x1' && y0 >= y0' && y1 <= y1'


let overlap mbr1 mbr2 =
    if intersect mbr1 mbr2 then
        let (x0, x1, y0, y1), (x0', x1', y0', y1') = mbr1, mbr2 in
        area
            ((if x0 > x0' then x0 else x0'),
            (if x1 > x1' then x1 else x1'),
            (if y0 > y0' then y0 else y0'),
            (if y1 > y1' then y1 else y1'))
    else
        0.