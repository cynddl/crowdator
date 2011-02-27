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


module type ElementSig =
sig
    type t
    val get_mbr : t -> Mbr.t
    val get_id : t -> int
end

module Rtree = functor (Element : ElementSig) ->
struct

    type t =
        | Node of (Mbr.t * t) list
        | Leaf of (Mbr.t * Element.t) list
        | Empty

    let empty = Empty
    
    let empty_node = (Mbr.empty, Empty)

    let rec size = function
        | Empty -> 0
        | Leaf f ->
            List.length f
        | Node n ->
            List.fold_left (+) 0 (List.map (fun (_, e) -> size e) n)

 
    let to_list rtree =
        let rec aux acc = function
            | Empty ->
                acc
            | Leaf f ->
                List.rev_append (List.map snd f) acc
            | Node n ->
                let liste = List.map (fun (_, e) -> aux [] e) n in
                (* fold_left ou fold_right ? *)
                List.fold_left (List.rev_append) acc liste
        in
            aux [] rtree


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
    
    let find_id id rtree =
        let rec aux_find = function
            | Empty -> None
            | Leaf f ->
                (
                    try
                        let _, elem = List.find (fun (_, e) -> Element.get_id e = id) f in
                        Some elem
                    with
                    | _ -> None
                )
            | Node n ->
                (
                    let rec aux = function
                        | [] ->
                            None
                        | (_, e) :: tl ->
                            (match aux_find e with
                                | None -> aux tl
                                | Some elem -> Some elem)
                    in aux n
                )
        in
            match aux_find rtree with
                | None -> raise (Invalid_argument "Id inconnu")
                | Some e -> e



    (** Découpage **)

    let mbr_nodes ns =
        if ns = [] then raise (Invalid_argument "Impossible de déterminer le mbr d'une liste vide");
        Mbr.add_list (List.map fst ns)
    
    let mbr_tree = function
        | Empty -> Mbr.empty
        | Leaf f ->
            mbr_nodes f
        | Node ns ->
            mbr_nodes ns

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
        if max (List.length list_l) (List.length list_r) < max (List.length list_b) (List.length list_t) then
            (* split selon l'axe x *)
            list_l, list_r
        else if max (List.length list_l) (List.length list_r) > max (List.length list_b) (List.length list_t) then
            list_b, list_t
        else begin
            if overlap list_l list_r < overlap list_b list_t then
                (* split selon l'axe x *)
                list_l, list_r
            else if overlap list_l list_r < overlap list_b list_t then
                (* split selon l'axe y *)
                list_b, list_t
            else
                (* split_smallest_coverage node ... *)
                list_l, list_r (* DEBUG *)
        end


    let split_node = linear_split



    (** Insertion **)

    let saturation_count = 8


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

    
    (*
        On insère un élément dans un rtree déja formé, en répercutant le mbr
        Pour cela, on renvoie une partie contenant les éléments modifiés et une
        autre contenant ceux inchangés
    *)
    
    let rec insert_aux (mbr, e) = function
        | Node n ->    
            (* On détermine le meilleur candidat *)
            let (_, elem_first), others, _ = partition_enlargment mbr n in
            (* On insère l'élément e dans cette branche *)
            (
                match insert_aux (mbr, e) elem_first with
                    (* Une branche entière touchée *)
                    | a, (_, Empty) ->
                        let ns = a :: others in
                        (mbr_nodes ns, Node ns), empty_node
                        (* Pas besoin de scinder *)
                    | a, b when List.length others + 2 < saturation_count ->
                        let ns = a :: b :: others in
                        (mbr_nodes ns, Node ns), empty_node
                        (* Il faut scinder *)
                    | a, b ->
                        let a0, b0 = split_node (a :: b :: others) in
                        (mbr_nodes a0, Node a0), (mbr_nodes b0, Node b0)
            )

        | Leaf f ->
            let new_elem = (mbr, e) :: f in
            if List.length f + 1 > saturation_count then
                let a, b = split_node new_elem
                in
                    (mbr_nodes a, Leaf a), (mbr_nodes b, Leaf b)
            else
                (mbr_nodes new_elem, Leaf new_elem), empty_node
            
        | Empty ->
            (mbr, Leaf [(mbr, e)]), empty_node
        
    let insert e t =
        match insert_aux (Element.get_mbr e, e) t with
            | (_, a), (_, Empty) ->
                a
            | (a, b) ->
                Node [a; b]
    
    (* Insertion d'une liste d'objets : on les insère un par un dans un nouveau r-tree *)
    let insert_list =
        List.fold_right insert
       

    (** Suppression d'éléments du R-tree **)
    
    (* L'id est ici unique ! *)
    let remove_id_list id =
        let rec aux = function
            | [] -> []
            | (m, e) :: tl when Element.get_id e = id -> tl
            | s :: tl -> s :: aux tl
        in
            aux

    let rec remove_id id = function
        | Empty ->
            Empty
        | Leaf f ->
            let new_list = remove_id_list id f in
            if new_list = [] then
                Empty
            else
                Leaf (new_list)
        | Node n ->
            let rec aux_remove = function
                | [] ->
                    []
                | (m, e) as s :: tl ->
                    let e0 = remove_id id e in
                    if e0 = Empty then
                        aux_remove tl
                    else if e = e0 then
                        s :: aux_remove tl
                    else
                        (mbr_tree e0, e0) :: tl
            in
                let new_list = aux_remove n in
                if new_list = [] then
                    Empty
                else
                    Node new_list
    
    (* Enlève tous les éléments vérifiant une condition donnée *)
    let rec remove_by_condition func = function
        | Empty ->
            Empty, []
        | Leaf f ->
            let l, removed = List.partition (fun (_, e) -> not (func e)) f in
            if l <> [] then
                Leaf l, (List.map snd removed)
            else
                Empty, (List.map snd removed) (* removed devrait etre [] *)
        | Node n ->
            let rec aux_aux acc_removed = function
                | [] ->
                    [], acc_removed
                | (m, e) :: tl ->
                    let sub_tree, removed = remove_by_condition func e in
                    if sub_tree = Empty then
                        aux_aux (List.rev_append removed acc_removed) tl
                    else
                        let sub_list, removed_suite = aux_aux [] tl in
                        (mbr_tree sub_tree, sub_tree) :: sub_list, List.rev_append removed_suite acc_removed
            in
                let sub_tree, removed = aux_aux [] n in
                if sub_tree = [] then
                    Empty, removed
                else
                    Node (sub_tree), removed


    (** Fonction(s) d'itération **)
    
    let iter_with_mbr func rtree =
        let rec aux = function
            | Empty ->
                ()
            | Leaf f ->
                List.iter func f
            | Node n ->
                List.iter (fun (_, e) -> aux e) n
        in
            aux rtree
    
    let iter func rtree =
        let rec aux = function
            | Empty ->
                ()
            | Leaf f ->
                List.iter (fun (_, e) -> func e) f
            | Node n ->
                List.iter (fun (_, e) -> aux e) n
        in
            aux rtree
    
    let iter_all func_mbr func_leaf rtree =
        let rec aux = function
            | Empty ->
                ()
            | Leaf f ->
                List.iter (fun (m, e) -> func_mbr (Element.get_mbr e); func_leaf e) f
            | Node n ->
                List.iter (fun (m, e) -> func_mbr m; aux e) n
        in
            aux rtree
    
    
    (** Fonctions de "mappage" **)
    
    let map func rtree =
        let rec aux = function
            | Empty ->
                Empty
            | Leaf f ->
                Leaf (List.map (fun (m, e) -> (Element.get_mbr (func e), func e)) f)
            | Node n ->
                Node (List.map (fun (m, e) -> (m, aux e)) n)
        in
            aux rtree

    let map_by_id func id rtree =
        let rec aux = function
            | Empty ->
                Empty
            | Leaf f ->
                Leaf (List.map
                    (fun (m, e) -> if Element.get_id e = id then
                        (Element.get_mbr (func e), func e)
                    else (m, e))
                f)
            | Node n ->
                Node (
                    let rec aux_aux = function
                        | [] -> []
                        | (m, e) as s :: tl ->
                            if e = aux e then
                                (m, aux e) :: tl
                            else
                                s :: aux_aux tl
                    in aux_aux n
                )
        in aux rtree
end