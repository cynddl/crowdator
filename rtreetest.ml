open Rtree
open Graphics2D
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
    
let rec random_rtree = function
    | 0 -> Empty
    | n -> insert (random_mbr (), n) (random_rtree (n-1))

let rec random_rtree_small = function
    | 0 -> Empty
    | n -> insert (random_mbr_small (), n) (random_rtree_small (n-1))

let _ =
    Random.self_init ();
    auto_synchronize true;    
    
    Unix.sleep 1;
    Primitives.wait 100;
    let test = random_rtree_small 100 in 
    Printf.printf "%i\n" (size test);
    display_rtree test;
    Unix.sleep 1;
    Primitives.wait 1000;

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
    Primitives.wait 3000;*)
    

    print_int 42