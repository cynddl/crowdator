(**
    Ce module permet de calculer précisément le temps d'exécution d'une fonction
    donnée, de réaliser des comparaisons entre plusieurs fonctions. Les résultats
    sont affichés clairement sur la sortie standard.
**)


open Unix


(* Opérations sur le type Unix.process_times *)

let ( -*> ) a b =
    {
        tms_utime = b.tms_utime -. a.tms_utime;
        tms_stime = b.tms_stime -. a.tms_stime;
        tms_cutime = b.tms_cutime -. a.tms_cutime;
        tms_cstime = b.tms_cstime -. a.tms_cstime
    }

let ( **> ) l a =
    {
        tms_utime =  l *. a.tms_utime;
        tms_stime =  l *. a.tms_stime;
        tms_cutime = l *. a.tms_cutime;
        tms_cstime = l *. a.tms_cstime
    }


(* Appelle n fois un fonction passée en argument. *)

let iter_func f n () =
    let rec aux = function
        | 0 -> ()
        | n -> f (); aux (n-1)
    in aux n


(* Calcule le temps d'exécution d'une fonction donnée. *)

let count_function f = 
    let t0 = Unix.times () in
    f ();
    let t1 = Unix.times () in
    t0 -*> t1

let count_function_with_result f = 
    let t0 = Unix.times () in
    let r = f () in
    let t1 = Unix.times () in
    t0 -*> t1, r


(* Calcule un temps moyen d'exécution d'une fonction. *)

let average_count_function f n =
    (1. /. (float_of_int n)) **> (count_function (iter_func f n))





(*
    Affiche sur la sortie standard le temps d'exécution à partir d'un type
    Unix.process_times.
    Un argument facultatif label permet de spécifier une "légende" à placer
    devant les temps d'exécution.
*)

let print_process_time ?(label = "") tms =
    Printf.printf "%s%fs user %fs system\n"
        (if label <> "" then label ^ " : " else "")
        tms.tms_utime
        tms.tms_stime;
    flush Pervasives.stdout


(* Affiche le temps passé entre deux temps de type Unix.process_time *)

let print_elapsed_time t0 t1 =
    print_process_time (t0 -*> t1)


(* Affiche sur la sortie standard le temps d'exécution d'une fonction. *)
    
let print_function_time ?(label = "") f =
    let time = count_function f in
    print_process_time ~label time

let print_function_time_with_result ?(label = "") f =
    let time, result = count_function_with_result f in
    print_process_time ~label time;
    result


(* Affiche sur la sortie standard le temps moyen d'exécution d'une fonction. *)

let print_average_time ?(label = "") f n =
    let time = average_count_function f n in
    print_process_time ~label time


(*let _ =
    let f () =
        ignore( Array.init 10000000 (fun i -> i) )
    
    in
    
    let f_random r () =
        f ()
    
    in
        print_function_time ~label:"f 3   " (f_random 3);
        print_function_time ~label:"f 4   " (f_random 4);
        print_function_time ~label:"f 42  " (f_random 42);
        print_function_time ~label:"f 23  " (f_random 23);
        print_function_time ~label:"f 3   " (f_random 3);
        print_function_time ~label:"f 3   " (f_random 3);
        print_function_time ~label:"f 3   " (f_random 3);
        print_function_time ~label:"f 3   " (f_random 3);
        
        print_average_time ~label:"f mean" f 10*)