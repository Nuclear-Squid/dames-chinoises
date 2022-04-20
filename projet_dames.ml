z#! /bin/env ocaml

(* ···---————————————————————---··· *)
(* <==== Définition des types ====> *)
(* ···---————————————————————---··· *)
type dimension = int (* Taille du plateau *)
type case = int * int * int (* Position d'une case sur le plateau *)
type vecteur = int * int * int (* Écart entre deux cases *)

(* La couleur d'un pion *)
type couleur = Vert | Jaune | Rouge | Noir | Bleu | Marron (* Les couleurs des joueurs *)
             | Libre          (* case vide *)
             | Code of string (* Chaine de 3 caractères *)

type case_coloree = case * couleur (* Position et couleur d'une case *)
(* type configuration = case_coloree list * couleur list * dimension (1* état du plateau *1) *)

(* c'était vraiment trop demandé d'en faire un struct putain?? *)
type configuration = {
    cases: case_coloree list;
    coul_joueurs: couleur list;
    dim_plateau: dimension;
}
			    
(* Les différentes façons de déplacer un pion *)
type coup = Du of case * case (* case départ -> case arrivé *)
          | Sm of case list   (* case départ -> chemin empreinté *)

(* ···---——————————————————---··· *)
(* <==== fonctions de debug ====> *)
(* ···---——————————————————---··· *)

(* Convertis une couleur en une chaine de caractères *)
let string_of_couleur (coul: couleur): string =
	match coul with
	| Libre  -> " . "
	| Code s -> s  
	| Vert   -> " V "
	| Jaune  -> " J "
	| Rouge  -> " R "
	| Noir   -> " N "
	| Bleu   -> " B "
	| Marron -> " M "

let string_of_case_coloree (((i, j, k), coul): case_coloree): string =
    "(" ^ string_of_int i ^ ", " ^ string_of_int j ^ ", " ^ string_of_int k ^ "," ^ string_of_couleur coul ^ ")"

let string_of_case_coloree_list (l: case_coloree list): string =
    let rec loop (l: case_coloree list) (str: string): string =
        match l with
        | [] -> str ^ "]"
        | c :: suite -> match suite with 
            | [] -> str ^ string_of_case_coloree c ^ "]"
            | _  -> loop suite (str ^ string_of_case_coloree c ^ "; ")
    in
    loop l "["

let string_of_int_list (l: int list): string =
    let rec loop (l: int list) (str: string): string =
        match l with
        | [] -> str ^ "]"
        | elem :: suite -> match suite with
            | [] -> str ^ string_of_int elem ^ "]"
            | _  -> loop suite (str ^ string_of_int elem ^ "; ")
    in
    loop l "["

let string_of_case ((i, j, k): case): string =
    "(" ^ string_of_int i ^ ", " ^ string_of_int j ^ ", " ^ string_of_int k ^ ")"

let string_of_case_list (l: case list): string = 
    let rec loop (l: case list) (str: string): string =
        match l with
        | [] -> str ^ "]"
        | c :: suite -> match suite with
            | [] -> str ^ string_of_case c ^ "]"
            | _  -> loop suite (str ^ string_of_case c ^ "; ")
    in
    loop l "["

let string_of_coul_list (l: couleur list): string =
    let rec loop (l: couleur list) (str: string): string =
        match l with 
        | [] -> str ^ "]"
        | coul :: suite -> match suite with
            | [] -> str ^ string_of_couleur coul ^ "]"
            | _  -> loop suite (str ^ string_of_couleur coul ^ "; ")
    in
    loop l "["

(* ···---————————————————————————---··· *)
(* <==== Définition des fonctions ====> *)
(* ···---————————————————————————---··· *)

(* <==== 1.1 Le plateau et les coordonées des cases ====> *)
(* Vérifie qu'une coordonné d'une case est possible *)
let indice_valide (x: int) (dim: dimension): bool = (x >= -2*dim) && (x <= 2*dim)


(* Vérifie que la case donnée est valide *)
let est_case ((i,j,k): case): bool = (i+j+k=0)


(* Vérifie qu'une une case est dans le losange nord-sud *)
let est_dans_losange ((i, j, k): case) (dim: dimension): bool =
    indice_valide i dim && (j >= -dim) && (j <= dim) && (k >= -dim) && (k <= dim)


(* Vérifie qu'une case est bien sur le plateau *)
let est_dans_etoile ((i, j, k): case) (dim: dimension): bool =
    (indice_valide i dim && (j >= -dim) && (j <= dim) && (k >= -dim) && (k <= dim)) ||
    ((i >= -dim) && (i <= dim) && indice_valide j dim && (k >= -dim) && (k <= dim)) ||
    ((i >= -dim) && (i <= dim) && (j >= -dim) && (j <= dim) && indice_valide k dim)


let rec tourner_case (m: int) ((i, j, k): case): case =
    match m with
    | 0 -> (i, j, k)
    | _ -> tourner_case (m-1) (-j, -k, -i)


(* Déplace une case celon un vecteur donné *)
let translate ((i, j, k): case) ((x, y, z): vecteur): case = (i+x, j+y, k+z)


(* Calcule l'écart entre deux cases donnés *)
let diff_case ((i, j, k): case) ((x, y, z): case): vecteur = (x-i, y-j, z-k)


(* <==== Préparation des coups simples: déplacements
   unitaires et sauts simples ====> *)

(* Le carre permet de faire la valeur absolue et donner une valeur beaucoup
   trop élevé au moment de la somme si on a autre chose que 0 et 1 *)
let sont_cases_voisines ((i, j, k): case) ((x, y, z): case): bool =
    let carre (x: int): int = x*x in
    let somme_carre ((a, b, c): case): int = carre a + carre b + carre c
    in
    somme_carre(diff_case (i, j, k) (x, y, z)) = 2

(* Renvoie l'écart entre deux cases sous la forme d'un vecteur unitaire et d'une distance *)
let vec_et_dist ((i, j, k): case) ((x, y, z): case): (vecteur * int) =
    let abs (n: int): int = if n > 0 then n else (-n) in
    let dist ((i, j, k): vecteur): int = ((abs i) + (abs j) + (abs k)) / 2 in
    let vect_unit ((i, j, k): vecteur) (d: int) = ((i / d), (j / d), (k / d)) in
    ( (vect_unit (diff_case(i, j, k) (x, y, z)) (dist(diff_case(i, j, k) (x, y, z)))),
        (dist(diff_case(i, j, k) (x, y, z))) )


(* Renvoie le milieu de deux cases données *)
(* None si les deux cases ne sont pas alligné ou si le milieu n'est pas valable *)
let calcule_pivot ((i, j, k): case) ((x, y, z): case) =
    let rec est_pair (n: int) = match n with (* comment ça `mod` existe ? *)
        | 0            -> true
        | 1            -> false
        | x when x < 0 -> est_pair (-x)
        | _            -> est_pair (n-2)
    in
    let alligne ((i, j, k): case) ((x, y, z): case): bool =
        (i = x) || (j = y) || (k = z)
    and milieu_existe ((i, j, k): case): bool =
        (* (i mod 2 = 0) && (j mod 2 = 0) && (k mod 2 = 0) *)
        est_pair i && est_pair j && est_pair k
    in
    if not (alligne (i, j, k) (x, y, z)) then None
    else if not (milieu_existe(diff_case (i, j, k) (x, y, z))) then None
    else Some ( ((i + x) / 2), ((j + y) / 2), ((k + z) / 2) )


(* Envoie le premier élément de la liste à la fin *)
let tourner_liste (l: 'a list): 'a list =
    match l with
    | []            -> []
    | elem :: suite -> suite @ [elem]


(* Renvoie le dernier élément d'une liste *)
let rec der_liste (l: 'a list): 'a =
    match l with
    | []            -> failwith "Liste vide passé à 'der_liste'"
    | elem :: suite -> match suite with
        | [] -> elem
        | _  -> der_liste suite


(* Rajoute n case à droite de la case donnée (sur l'axe i) *)
let remplir_segment (n: int) ((i, j, k): case): case list =
    let rec loop (n: int) ((i, j, k): case) (l: case list): case list =
        match n with
        | 0 -> l
        | _ -> loop (n-1) (i, j+1, k-1) (l @ [(i, j, k)])
    in
    loop n (i, j, k) []


(* Génère un triangle de n cases de côté avec la pointe vecticale vers le bas *)
let remplir_triangle_bas (n: int) (coin_sup_gauche: case): case list =
    let rec ligne (n: int) ((i, j, k): case) (rv_liste: case list): case list =
        match n with
        | 0 -> rv_liste
        | _ -> ligne (n-1) ((i-1), (j+1), k) (rv_liste @ remplir_segment n (i,j,k))
    in
    ligne n coin_sup_gauche []


(* Génère un triangle de n cases de côté avec la pointe vecticale vers le haut *)
let remplir_triangle_haut (n: int) (coin_inf_gauche: case): case list =
    let rec ligne (n: int) ((i, j, k): case) (rv_liste: case list): case list =
        match n with
        | 0 -> rv_liste
        | _ -> ligne (n-1) ((i+1), j, (k-1)) (rv_liste @ remplir_segment n (i,j,k))
    in
    ligne n coin_inf_gauche []


(* On va faire du coloriage les enfants !!!!! *)
let colorie (liste_case: case list) (coul: couleur): case_coloree list =
    let rec loop (liste_case: case list) (coul: couleur)
                 (rv_liste: case_coloree list): case_coloree list =
        match liste_case with 
        | []         -> rv_liste
        | c :: suite -> loop suite coul (rv_liste @ [(c, coul)])
    in
    loop liste_case coul []


(* Tourne le plateau pour mettre le joueur qui doit jouer en bas *)
let tourner_configuration (config: configuration): configuration =
    let len (liste: 'a list): int =
        let rec loop (liste: 'a list) (n: int): int =
            match liste with
            | [] -> n
            | elem :: suite -> loop suite (n+1)
        in
        loop liste 0
    in
    let sixieme_tours: int = match len config.coul_joueurs with
        | 0 -> 6
        | n -> 6 / n
    in
    let rec tourne_cases_config (cases_liste: case_coloree list)
                                (rv_liste: case_coloree list): case_coloree list =
        match cases_liste with
        | [] -> rv_liste
        | (c, coul) :: suite -> tourne_cases_config suite (rv_liste @ [(tourner_case sixieme_tours c), coul])
    in {
        cases = tourne_cases_config config.cases [];
        coul_joueurs = tourner_liste config.coul_joueurs;
        dim_plateau = config.dim_plateau
    }


(* Initialiser le plateau *)
let remplir_init (joueurs: couleur list) (dim: dimension): configuration =
    let genere_triangle (coul: couleur) (dim: dimension): case_coloree list =
        colorie (remplir_triangle_bas dim (-dim-1, 1, dim)) coul
    in
    let rec loop (joueurs: couleur list) (liste_couleurs: couleur list)
                 (dim: dimension) (rv_config: configuration): configuration =
        match joueurs with
        | []             -> rv_config
        | coul :: suite  -> loop suite (tourner_liste liste_couleurs) dim (tourner_configuration {
                                cases = rv_config.cases @ genere_triangle coul dim;
                                coul_joueurs = liste_couleurs;
                                dim_plateau = dim
                            })
    in
    loop joueurs joueurs dim { cases = []; coul_joueurs = []; dim_plateau = dim }


(* <==== 2.2 Recherche et suppression de case dans une configuration ====> *)

(* Verifie si une valeur est présente dans une liste de tuple de 2 éléments
   si oui, elle renvoie le deuxième élément du tuple, sinon c'est defaut *)
let rec associe (val_a_chercher: 'a) (liste: ('a * 'b) list): 'b option =
    match liste with
    | [] -> None
    | (val_a_tester, val_associe) :: suite -> (
        if val_a_chercher = val_a_tester then Some(val_associe)
        else associe val_a_chercher suite )


(* Renvoie la couleur d'une case donnée *)
let quelle_couleur (config: configuration) (c: case): couleur =
    match associe c config.cases with
    | Some(coul) -> coul
    | None       -> Libre


(* Supprime une case de la config *)
let supprime_dans_config (config: configuration) (c: case): configuration =
    let rec liste_sans_une_case (case_viree: case) (liste: case_coloree list)
                        (rv_list: case_coloree list): case_coloree list =
        match liste with
        | [] -> rv_list
        | (c, coul) :: suite ->
                if c = case_viree then
                    liste_sans_une_case c suite rv_list
                else liste_sans_une_case case_viree suite (rv_list @ [c, coul])
    in {
        cases = liste_sans_une_case c config.cases [];
        coul_joueurs = config.coul_joueurs;
        dim_plateau = config.dim_plateau
    }
    

(* Verifie que le saut est valide *)
let rec est_saut_multiple (config: configuration) (case_liste: case list): bool =
    (* Verifie que toutes les cases soient bien libres *)
    let est_libre_seg (config: configuration) (c1: case) (c2: case): bool =
        let rec loop (config: configuration) ((i, j, k): case)
                     (((x, y, z), distance): vecteur * int): bool =
            match distance with
            | 1 -> true
            | n -> match quelle_couleur config (i+x, j+y, k+z) with
                | Libre -> loop config (i+x, j+y, k+z) ((x, y, z), distance-1)
                | _     -> false
        in
        loop config c1 (vec_et_dist c1 c2)
    in
    (* Vérifie que le saut d'une case a une autre est valide *)
    let est_saut (config: configuration) (c1: case) (c2: case): bool =
        let pivot_valide (c1: case) (c2: case): bool =
            match calcule_pivot c1 c2 with
            | None -> false
            | Some(pivot) -> ( 
                quelle_couleur config pivot <> Libre &&
                est_libre_seg config c1 pivot &&
                est_libre_seg config pivot c2 )
        in
        pivot_valide c1 c2 &&
        quelle_couleur config c2 = Libre &&
        est_dans_etoile c1 config.dim_plateau && (* le losange suffit non ? *)
        est_dans_losange c2 config.dim_plateau
    in
    (* Vérifie que tous les sauts sont valides *)
    match case_liste with
    | [] -> failwith "Liste vide passé à `est_saut_multiple`"
    | c :: [] -> true
    | c1 :: c2 :: suite -> (
        if est_saut config c1 c2 then est_saut_multiple config (c2 :: suite)
        else false )


(* <==== 2.3 Jouer un coup unitaire ====> *)
let est_coup_valide (config: configuration) (action: coup): bool =
    let est_bon_joueur (config: configuration) (c: case): bool =
        match associe c config.cases with
        | None -> false (* pas de cases coloré sur le plateau à cet endroit *)
        | Some(coul) -> coul == List.hd(config.coul_joueurs)
            (* vérifie que la case appartient au joueur dont c'est le tour *)
    in
    match action with
    | Sm(liste_case) -> est_saut_multiple config liste_case
    | Du(c1, c2) -> ( (est_bon_joueur config c1) &&
        (quelle_couleur config c2 == Libre) &&
        (sont_cases_voisines c1 c2) &&
        (est_dans_losange c2 config.dim_plateau) )


(* Joue un coup donné *)
let appliquer_coup (config: configuration) (action: coup): configuration =
    let ajoute_case (config: configuration) (c: case): configuration =
        tourner_configuration {
        cases = (c, List.hd(config.coul_joueurs)) :: config.cases;
        coul_joueurs = config.coul_joueurs;
        dim_plateau = config.dim_plateau
    }
    in
    match action with
    | Sm(liste_case) -> ajoute_case (supprime_dans_config config (List.hd liste_case)) (der_liste liste_case)
    | Du(c1, c2) -> ajoute_case (supprime_dans_config config c1) c2


(* On nous dit de mettre cette fonction a jour mais elle existe pas donc je
   vais l'inventer en entier *)
(* Vérifie qu'un coup soit valide, si oui, le joue *)
let mettre_a_jour_configuration (config: configuration) (action: coup): configuration =
    if est_coup_valide config action then appliquer_coup config action
    else failwith "Ce coup est invalide, rejoue fdp"


(* ···---———————————————————---··· *)
(* <==== Vérifier une partie ====> *)
(* ···---———————————————————---··· *)

(* Donne le score du joueur dont c'est le tour *)
let score (config: configuration): int =
    let filtre (coul_joueur: couleur) ((c, coul): case_coloree): bool =
        coul = coul_joueur
    and get_score (((i, j, k), coul): case_coloree): int = i
    and sum (l: int list): int = List.fold_left ( + ) 0 l
    in
    let cases_joueur: case_coloree list = List.filter (filtre (List.hd config.coul_joueurs)) config.cases
    in
    let liste_score: int list = List.map get_score cases_joueur
    in
    sum liste_score


(* Vérifie si le joueur dont c'est le tour a gagné *)
let gagne (config: configuration): bool =
    let score_gagnant (dim: dimension): int =
        let cases = remplir_triangle_haut dim (dim + 1, dim, 1)
        in
        let score_cases = List.map (fun (i, j, k) -> i) cases
        in
        List.fold_right ( + ) 0 score_cases
    in
    score config = score_gagnant dim


(* (1* Les boucles c'est de l'impératif et l'impératif c'est mal *1) *)
(* let score_gagnant (dim: dimension): int = *)
(*     let score = ref 0 *)
(*     and fac = ref 1 *)
(*     in *)
(*     for i = dim * 2 downto dim + 1 *)
(*     do *)
(*         score := !score + (i * !fac); *)
(*         fac := !fac + 1; *)
(*     done; *)
(*     !score *)


(* ···---—————————————————————————---··· *)
(* <==== Affichage dans la console ====> *)
(* ···---—————————————————————————---··· *)


(* Affiche le plateau dans le term *)
let affiche_plateau (config: configuration): unit =
    (* transforme des coordonnees cartesiennes (x,y) en coordonnees de case (i,j,k) *)
    let transfo (x: int) (y: int) = (y, (x - y) / 2, (-x - y) / 2)
    in
    let rec affiche_ligne (config: configuration) (ligne: int) (colonne: int) (str: string): string =
        if colonne = (4 * config.dim_plateau) + 1 then str (* fin de ligne *)
        else
            let c: case = transfo colonne ligne in
            (* inter-case ou case hors de l'etoile *)
            if not ((ligne + colonne) mod 2 = 0) || not (est_dans_etoile c config.dim_plateau) then
                affiche_ligne config ligne (colonne + 1) (str ^ "   ")
            else (* case appartenant au plateau *)
                affiche_ligne config ligne (colonne + 1) (str ^ (string_of_couleur (quelle_couleur config c)))
    in
    let rec loop (ligne: int): unit =
        if ligne <> - 2 * config.dim_plateau - 1 then (
            Printf.printf "%s\n" (affiche_ligne config ligne (-4 * config.dim_plateau - 1) "");
            Printf.printf "\n\n"; (* sauter une ligne pour la présentation *)
            loop (ligne - 1)
        )
    in
    loop (2 * config.dim_plateau + 1)

;;


(* ···---—————---··· *)
(* <==== Tests ====> *)
(* ···---—————---··· *)



affiche_plateau (mettre_a_jour_configuration {
    cases = [((-4, 2, 2), Vert); ((-2, 0, 2), Vert); ((1, -2, 1), Vert); ((0, 0, 0), Vert); ((-1, 2, -1), Vert); ((2, 0, -2), Vert)];
    coul_joueurs = [Vert];
    dim_plateau = 3 } (Sm([(-4, 2, 2); (0, -2, 2); (2, -2, 0); (-2, 2, 0); (0, 2, -2); (4, -2, -2)])));

(* affiche_plateau (mettre_a_jour_configuration { *)
(*     cases = [((-4, 2, 2), Vert); ((-2, 0, 2), Vert); ((1, -2, 1), Vert); ((0, 0, 0), Vert); ((-1, 2, -1), Vert); ((2, 0, -2), Vert)]; *)
(*     coul_joueurs = [Vert]; *)
(*     dim_plateau = 3 } (Sm([(-4, 2, 2); (0, -2, 2); (2, -2, 0); (-2, 2, 0); (0, 2, -2); (5, -3, -2)]))); *)


Printf.printf "%i\n" (score_joueur {
    cases = [((-4, 2, 2), Vert); ((-2, 0, 2), Vert); ((1, -2, 1), Vert); ((0, 0, 0), Vert); ((-1, 2, -1), Vert); ((2, 0, -2), Vert)];
    coul_joueurs = [Vert];
    dim_plateau = 3 });