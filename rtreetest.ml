open Rtree
(*open Graphics2D*)
open Primitives
open Unix


let random_mbr () =
    let x0 = Random.float_range 5. 25. in
    let x1 = Random.float_range 5. 25. in
    let y0 = Random.float_range 5. 25. in
    let y1 = Random.float_range 5. 25. in
    min x0 x1, max x0 x1, min y0 y1, max y0 y1
    
let random_mbr_small () =
    let x0 = Random.float_range 5. 25. in
    let x1 = Random.float_range x0 (x0 +. 2.) in
    let y0 = Random.float_range 5. 25. in
    let y1 = Random.float_range y0 (y0 +. 2.) in
    x0, x1, y0, y1


module Test = struct
    type t = {id:int; mbr:Mbr.t}
    
    let get_mbr x = x.mbr
    
    let get_id x = x.id
    
    let random_elem i =
        let r = random_mbr () in
        {id = i; mbr = r}
    
    let random_elem_small i =
        let r = random_mbr_small () in
        {id = i; mbr = r}

end

module RtreeTesting = Rtree(Test)

let rec random_rtree n0 =
    let rec aux acc = function
        | 0 ->
            acc
        | n ->
            aux (RtreeTesting.insert (Test.random_elem n) acc) (n-1)
    in
        aux RtreeTesting.empty n0

let random_rtree_small n0 =
    let rec aux acc = function
        | 0 ->
            acc
        | n ->
            aux (RtreeTesting.insert (Test.random_elem_small n) acc) (n-1)
    in
        aux RtreeTesting.empty n0

(*let display_test rtree =
    RtreeTesting.iter_all
        display_mbr
        (fun toto ->
            set_color blue;display_mbr (Test.get_mbr toto);set_color black
        )
        
    rtree*)

let _ =
    Random.self_init ();
    (*auto_synchronize true;*)
    
    let n =
        if Array.length Sys.argv > 1 then
            int_of_string (Sys.argv.(1))
        else
            1000
    in
    Printf.printf "Creating a R-tree with %i random element(s)\n" n;
    let tree = random_rtree_small n in
    Printf.printf "Our R-tree is here !\n\n";
    Printf.printf "R-tree size : %i\n" (RtreeTesting.size tree)
    
    (*Unix.sleep 1;
    Primitives.wait 1000;
    let test = random_rtree_small 5 in 
    display_test test;
    Unix.sleep 1;
    Primitives.wait 1000;
    
    let test = RtreeTesting.insert (Test.random_elem_small 42) test in
    display_test test;
    Unix.sleep 1;
    Primitives.wait 1000;
    
    let test = RtreeTesting.insert (Test.random_elem_small 42) test in
    display_test test;
    Unix.sleep 1;
    Primitives.wait 1000;
    
    let test = RtreeTesting.insert (Test.random_elem_small 42) test in
    display_test test;
    Unix.sleep 1;
    Primitives.wait 1000;
    
    let test = RtreeTesting.insert (Test.random_elem_small 42) test in
    display_test test;
    Unix.sleep 1;
    Primitives.wait 1000;
    
    let test = RtreeTesting.insert (Test.random_elem_small 42) test in
    display_test test;
    Unix.sleep 1;
    Primitives.wait 2000;
    
    let test = RtreeTesting.insert (Test.random_elem_small 42) test in
    display_test test;
    Unix.sleep 1;
    Primitives.wait 2000;*)
    

    (*let test = insert ((0., 1., 0., 1.), 1) Empty in
    let test = insert ((1., 1., 2., 2.), 2) test in
    let test = insert ((9., 10., 0., 1.), 3) test in
    let test = insert ((9., 10., 9., 10.), 4) test in
    let test = insert ((0., 1., 9., 10.), 5) test in
    let test = insert ((8., 9., 1., 2.), 6) test in
    display_rtree test;
    Unix.sleep 1;
    Primitives.wait 1000;
    
    let test = insert ((6., 7., 1., 2.), 6) test in
    clear_graph ();
    display_rtree test;
    Unix.sleep 1;
    Primitives.wait 1000;
    
    let test = insert ((5., 6., 1., 2.), 6) test in
    clear_graph ();
    display_rtree test;
    Unix.sleep 1;
    Primitives.wait 1000;
    
    let test = insert ((4., 5., 5., 7.), 6) test in
    clear_graph ();
    display_rtree test;
    Unix.sleep 1;
    Primitives.wait 3000*)