open Rtree
open Primitives
open Timetester
open Unix


let random_mbr () =
    let x0 = Random.float_range 5. 25000. in
    let x1 = Random.float_range 5. 25000. in
    let y0 = Random.float_range 5. 25000. in
    let y1 = Random.float_range 5. 25000. in
    min x0 x1, max x0 x1, min y0 y1, max y0 y1
    
let random_mbr_small () =
    let x0 = Random.float_range 5. 25000. in
    let x1 = Random.float_range x0 (x0 +. 0.01) in
    let y0 = Random.float_range 5. 25000. in
    let y1 = Random.float_range y0 (y0 +. 0.01) in
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




let random_rtree n0 =
    let rec aux acc = function
        | 0 ->
            acc
        | n ->
            aux (RtreeTesting.insert (Test.random_elem_small n) acc) (n-1)
    in
        aux RtreeTesting.empty n0
        
let random_rtree_and_list n0 =
    let rec aux acc = function
        | 0 ->
            acc
        | n ->
            let new_elem = Test.random_elem_small n in
            let tree, liste = acc in
            aux ((RtreeTesting.insert new_elem tree) , new_elem :: liste) (n-1)
    in
        aux (RtreeTesting.empty, []) n0



let _ =
    Random.self_init ();
    
    let n =
        if Array.length Sys.argv > 1 then
            int_of_string (Sys.argv.(1))
        else
            1000
    in
    
    Printf.printf "Creating a R-tree with %i random element(s)\n" n;
    let tree, liste = random_rtree_and_list n in
    
    
    let box = 10., 100., 10., 100. in
    
    let f0 () =
        List.length (RtreeTesting.find_mbr box tree)
    in
    
    let f1 () =
        List.length (List.filter (fun e -> Mbr.intersect box (Test.get_mbr e)) liste)
    in
    
    let s0 = Timetester.print_function_time_with_result ~label:"R-tree" f0 in
    let s1 = Timetester.print_function_time_with_result ~label:"Liste " f1 in
    
    Printf.printf "Rtree : %i -- Liste : %i\n" s0 s1
    
    
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