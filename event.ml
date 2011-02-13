(* Gestion des évenements / Liens utilisateur - programme*)


(* Pour les mouvements de l'écran *)
type direction =
    | Up
    | Down
    | Right
    | Left

type zoom_type =
    | In
    | Out
    
type action =
    | Move of direction
    | Zoom of zoom_type
    | Quit
    | NoDisplay
    | Nothing


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