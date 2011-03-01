(* Gestion des évenements / Liens utilisateur - programme*)


(* Directions de déplacement de l'écran *)

type direction =
    | Up
    | Down
    | Right
    | Left


(* Zoom avant et arrière *)

type zoom_type =
    | In
    | Out
    

(*
    Actions possibles :
     - celles liées à l'écran (déplacement de la zone active)
     - fermeture du programme
     - arrêt de l'affichage (passage en mode non-graphique)
     - aucune action
*)

type action =
    | Move of direction
    | Zoom of zoom_type
    | Quit
    | NoDisplay
    | Nothing


(*
    Renvoie l'action associée à une touche du clavier. Dans le cas où la touche
    ne sert à "rien", l'action est Nothing
*)

let parse_keypressed = function
    | 'z' -> Move(Up)
    | 's' -> Move(Down)
    | 'q' -> Move(Left)
    | 'd' -> Move(Right)
    | '+' -> Zoom(In)
    | '-' -> Zoom(Out)
    | '\027' -> Quit
    | '\r' -> NoDisplay
    | _ -> Nothing