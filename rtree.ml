(**
    Un R-tree est un arbre permettant des recherches spatiales optimisées.
    Il se base sur un découpage par rectanges minimaux (MBR), permettant un
    accès rapide aux feuilles, qui peuvent contenir plusieurs éléments.
    L'inconvénient majeur des R-tree est le recouvrement potentiel de certaines
    branches, qui peut augmenter la complexité.
    
    Complexité :
     - pour la recherche, O(log n) sans recouvrement et jusqu'à O(n) au pire
       (recouvrement totale, i.e. un seul noeud)
     - insertion, O(log n) si le noeud final n'est pas saturé ;  sinon, il faut
       découper le noeud d'insertion... possible en temps quadratique
**)

open Mbr

type 'a t =
    | Node of (Mbr.t * 'a t) list
    | Leaf of (Mbr.t * 'a) list
    | Empty

let rec size = function
    | Empty -> 0
    | Leaf f ->
        List.length f
    | Node n ->
        List.fold_left (+) 0 (List.map (fun (_, e) -> size e) n)


(* Filtre les éléments d'une liste qui intersectent un rectangle donné. *)
let filter_mbr m =
    List.filter (fun (m0, _) -> Mbr.intersect m0 m)

(* Renvoie la liste de tous les éléments d'un R-tree intersectant un MBR donné *)
let rec find_mbr m = function
    | Empty -> []
    | Leaf f ->
        (* On filtre tous les éléments de la feuille *)
        filter_mbr m f
    | Node n ->
        (* On filtre récursivement tous les éléments du noeud. On sélectionne 
        avant tout les éléments que l'on doit parcourir. *)
        let aux (mbr, e) = match (Mbr.intersect m mbr) with
            | false -> []
            | true -> find_mbr m e
        in
        List.concat (List.map aux n)

(** Découpage **)

let mbr_nodes ns =
    if ns = [] then raise (Invalid_argument "Impossible de déterminer le mbr d'une liste vide");
    Mbr.add_list (List.map fst ns)

let overlap list1 list2 =
    if list1 = [] || list2 = [] then 0.
    else
        let mbr1 = mbr_nodes list1
        and mbr2 = mbr_nodes list2 in
        Mbr.overlap mbr1 mbr2

(* L'algorithme développé ci-dessous n'est pas celui proposé originellement
   par Guttman *)
let linear_split node =
    let l, r, b, t = mbr_nodes node in
    let rec aux = function
        | [] -> [], [], [], []
        | ((xl, xh, yl, yh), _) as s :: tl ->
            let list_l, list_r, list_b, list_t = aux tl in
            let list_l' = 
                if xl -. l < r -. xh then
                    s :: list_l
                else
                    list_l
            and list_r' =
                if xl -. l < r -. xh then
                    list_r
                else
                    s :: list_r
            and list_b' =
                if yl -. b < t -. yh then
                    s :: list_b
                else
                    list_b
            and list_t' =
                if yl -. b < t -. yh then
                    list_t
                else
                    s :: list_t
            in (list_l', list_r', list_b', list_t')
    in
    let list_l, list_r, list_b, list_t = aux node in
    Printf.printf "%i %i %i %i\n" (List.length list_l) (List.length list_r) (List.length list_b) (List.length list_t);
    if max (List.length list_l) (List.length list_r) < max (List.length list_b) (List.length list_t) then
        (* split selon l'axe x *)
        (list_l, list_r)
    else begin
        (*if overlap list_l list_r < overlap list_b list_t then
            (* split selon l'axe x *)
            list_t, list_b
        else if overlap list_l list_r < overlap list_b list_t then
            (* split selon l'axe y *)
            list_l, list_r
        else
            (* split_smallest_coverage node ... *)
            list_l, list_r (* DEBUG *)*)
        list_t, list_b
    end

let split_node = linear_split

(** Insertion **)

let saturation_count = 5

let enlargment mbr mbr' =
    Mbr.area (Mbr.add mbr mbr') -. Mbr.area mbr

(* Renvoie un tuple contenant l'élément d'élargissement minimal, les autres
   éléments du noeud et l'augmentation en surface du mbr. *)
let rec partition_enlargment mbr = function
    | [] ->
        raise (Invalid_argument "Impossible de partager un noeud vide")
    | ((m, _) as e) :: [] ->
        e, [], enlargment m mbr
    | ((m, _) as e) :: tl ->
        let min', others', enlargment' = partition_enlargment mbr tl in
        let enlargment0 = enlargment m mbr in
        if enlargment0 < enlargment' || (enlargment0 = enlargment' && Mbr.area m < Mbr.area (fst min')) then
            e, tl, enlargment0
        else
            min', e :: others', enlargment'

let rec insert (mbr, e) = function
    | Node n ->
        (*---Cadre plus simple, nécessaire ???---
        let direct_candidates = List.filter (fun (m, _) -> is_in mbr m) n in
        (* L'élément est directement insersible dans un sous-noeud *)
        if List.length direct_candidates <> 0 then
            insert (mbr, e) (Leaf (List.hd direct_candidates))
        (* Il faut élargir les noeuds présents : on minimise l'élargissement possible *)
        else
        *)
        let first, others, _ = partition_enlargment mbr n in
        let mbr_first, elem_first = first in
        (* On insère l'élément e dans elem_first et on répercute le mbr *)
        let inserted_node = insert (mbr, e) elem_first in
        Node ((Mbr.add mbr_first mbr, inserted_node) :: others)

    | Leaf f ->
        let new_elem = (mbr, e) :: f in
        if List.length f + 1 > saturation_count then
            let a, b = split_node new_elem
            in
                Node [(mbr_nodes a, Leaf a); (mbr_nodes b, Leaf b)]
        else
            Leaf new_elem
            
    | Empty -> Leaf [(mbr, e)]