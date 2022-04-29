#! /bin/env ocaml

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

(* On a transformé le type configuration en un struct pour avoir des éléments nommés *)
type configuration = {
    cases: case_coloree list;
    coul_joueurs: couleur list;
    dim_plateau: dimension;
}
			    
(* Les différentes façons de déplacer un pion *)
type coup = Du of case * case (* case départ -> case arrivé *)
          | Sm of case list   (* case départ -> chemin empreinté *)


(* ···---————————————————---··· *)
(* <==== Casts en strings ====> *)
(* ···---————————————————---··· *)


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


let string_of_case ((i, j, k): case): string =
    "(" ^ string_of_int i ^ ", " ^ string_of_int j ^ ", " ^ string_of_int k ^ ")"


let string_of_case_coloree (((i, j, k), coul): case_coloree): string =
    "(" ^ string_of_int i ^ ", " ^ string_of_int j ^ ", " ^ string_of_int k ^ "," ^ string_of_couleur coul ^ ")"


let string_of_list_horizontal (convert: 'a -> string) (liste: 'a list): string =
    let rec loop (convert: 'a -> string) (liste: 'a list) (str: string): string =
        match liste with
        | [] -> str ^ "]"
        | elem :: suite -> match suite with
            | [] -> str ^ convert elem ^ "]"
            | _  -> loop convert suite (str ^ convert elem ^ "; ")
    in
    loop convert liste "["


let string_of_coup (cp: coup): string =
    match cp with
    | Du(c1, c2) -> "Du(" ^ string_of_case c1 ^ ", " ^ string_of_case c2 ^ ")"
    | Sm(l) -> "Sm(" ^ (string_of_list_horizontal string_of_case l) ^ ")"


let string_of_list_vertical (convert: 'a -> string) (liste: 'a list): string =
    let rec loop (convert: 'a -> string) (liste: 'a list) (str: string): string =
        match liste with
        | [] -> str ^ "]"
        | elem :: suite -> match suite with
            | [] -> str ^ convert elem ^ "\n]"
            | _  -> loop convert suite (str ^ convert elem ^ ";\n")
    in
    loop convert liste "[\n"


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
    | _ -> tourner_case (m-1) (-k, -i, -j)


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
    let rec est_pair (n: int) = 
        match n with (* comment ça `mod` existe ? *)
        | 0            -> true
        | 1            -> false
        | x when x < 0 -> est_pair (-x)
        | _            -> est_pair (n-2)
    in
    let milieu_existe ((i, j, k): case): bool =
        est_pair i && est_pair j && est_pair k
    in
    if not (milieu_existe(diff_case (i, j, k) (x, y, z))) then None
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


(* Associe une couleur donné à toutes les cases passé en argument *)
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
    

(* Vérifie que le saut d'une case a une autre est valide *)
let est_saut (config: configuration) (c1: case) (c2: case): bool =
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


(* Verifie que le saut est valide *)
let rec est_saut_multiple (config: configuration) (case_liste: case list): bool =
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
    | Sm(liste_case) -> est_saut_multiple (supprime_dans_config config (List.hd liste_case)) liste_case
    | Du(c1, c2) -> ( (est_bon_joueur config c1) &&
        (quelle_couleur config c2 == Libre) &&
        (sont_cases_voisines c1 c2) &&
        (est_dans_losange c2 config.dim_plateau) )


(* Joue un coup donné *)
let appliquer_coup (config: configuration) (action: coup): configuration =
    let ajoute_case (config: configuration) (c: case): configuration = {
        cases = (c, List.hd(config.coul_joueurs)) :: config.cases;
        coul_joueurs = config.coul_joueurs;
        dim_plateau = config.dim_plateau
    }
    in
    match action with
    | Sm(liste_case) -> ajoute_case (supprime_dans_config config (List.hd liste_case)) (der_liste liste_case)
    | Du(c1, c2) -> ajoute_case (supprime_dans_config config c1) c2


(* Vérifie qu'un coup soit valide, si oui, le joue *)
let mettre_a_jour_configuration (config: configuration) (action: coup): configuration =
    if est_coup_valide config action then tourner_configuration (appliquer_coup config action)
    else failwith "Ce coup est invalide, rejoue."


(* ···---——————————————————————---··· *)
(* <==== Affichage dans le term ====> *)
(* ···---——————————————————————---··· *)


(* transfo transforme des coordonnees cartesiennes (x,y) en coordonnees de case (i,j,k) *)
let transfo x y = (y, (x-y)/2,(-x-y)/2)


(* On a dû changer un peu ces fonctions pour qu'elles marchent bien *)
(* affiche dans le term une ligne du plateau en particulier *)
let rec affiche_ligne (config: configuration) (n: int) (m: int): string =
    if m = (4 * config.dim_plateau) + 1 then "" (* fin de ligne *)
    else
        let c: case = transfo m n in
        (* ceci est une inter-case (case inutile d'un damier) ou hors de l'etoile *)
        if not ((n + m) mod 2 = 0) || not (est_dans_etoile c config.dim_plateau) then
            "   " ^ affiche_ligne config n (m + 1)
        else (* ceci est une case ou bien en dehors du plateau *)
            (string_of_couleur (quelle_couleur config c)) ^ affiche_ligne config n (m + 1)


(* affiche dans le term le plateau en entier *)
let affiche (config: configuration): unit =
    let rec affiche_aux n =
        if n = -2 * config.dim_plateau - 1 then () (* early return *)
        else (
            print_endline (affiche_ligne config n (-4 * config.dim_plateau - 1));
            print_endline "\n";
            affiche_aux (n - 1)
        )
    in
    affiche_aux (2 * config.dim_plateau + 1)


(* ···---———————————————————---··· *)
(* <==== Vérifier une partie ====> *)
(* ···---———————————————————---··· *)


let score (config: configuration): int =
    let uuu (coul_joueur: couleur) (((i, j, k), coul): case_coloree): int option =
        if coul = coul_joueur then Some(i)
        else None
    in
    let liste_score = List.filter_map (uuu (List.hd config.coul_joueurs)) config.cases
    in
    List.fold_right ( + ) liste_score 0


(* Vérifie si le joueur dont c'est le tour a gagné *)
let gagne (config: configuration): bool =
    let score_gagnant (dim: dimension): int =
        let cases = remplir_triangle_haut dim (dim + 1, dim, 1)
        in
        let score_cases = List.map (fun (i, j, k) -> i) cases
        in
        List.fold_right ( + ) score_cases 0
    in
    score config = score_gagnant config.dim_plateau


(* J'ai jamais été aussi déçu de faire du code qui marche *)
let est_partie (config: configuration) (liste_coups: coup list): couleur =
    let nb_joueurs = List.length config.coul_joueurs
    and nb_tours = List.length liste_coups
    in
    let i_der_tours = List.init nb_joueurs (fun n -> nb_tours - n - 1)
    and coups_enumerated = List.mapi (fun i c -> (i, c)) liste_coups
    in
    let n_premiers_tours_par_joueurs =
        List.rev_map (fun i_max ->
            (List.partition (fun (i, _) -> i <= i_max) coups_enumerated)
        ) i_der_tours
    in
    let n_premier_tours_par_joueurs2 =
        List.map (fun (l, _) ->
            (List.map (fun (_, action) -> action) l)
        ) n_premiers_tours_par_joueurs
    in
    let dernier_etat_joueurs =
        List.map (
            List.fold_left mettre_a_jour_configuration config 
        ) n_premier_tours_par_joueurs2
    and liste_sixieme_tours = List.init (nb_joueurs - 1) succ
    in
    let der_etat_joueurs_tourne =
        List.map (fun plateau_joueur ->
            (List.fold_right (fun _ plateau ->
                tourner_configuration plateau) liste_sixieme_tours plateau_joueur)
        ) dernier_etat_joueurs
    in
    affiche (der_liste der_etat_joueurs_tourne);
    let ont_gagne_joueur = List.map gagne der_etat_joueurs_tourne
    in
    let ont_gagne_indices = List.mapi (fun i b -> (b, i)) ont_gagne_joueur
    in
    let ont_gagne_pour_de_vrai = List.filter (fun ((b, _): bool * 'a) -> b) ont_gagne_indices
    in
    match ont_gagne_pour_de_vrai with
    | [] -> Libre
    | (_, i) :: suite -> List.nth config.coul_joueurs ((i + nb_tours) mod nb_joueurs)


(* ···---————————————————---··· *)
(* <==== Calcul des coups ====> *)
(* ···---————————————————---··· *)


let coups_possibles (config: configuration) (c: case): coup list =
    let depl_unit_possibles (config: configuration) (c: case): coup list =
        let cases_voisines =
            List.init 6 (fun n -> translate c (tourner_case n (1, -1, 0)))
        and coup_valide (c1: case) (c2: case): coup option = 
            if est_coup_valide config (Du(c1, c2)) then
                Some(Du(c1, c2))
            else
                None
        in
        List.filter_map (coup_valide c) cases_voisines

    and sauts_mult_possibles (config: configuration) (c: case): coup list =
        let saut_valides (config: configuration) (chemin: case list)
                        (pivot: case): case list option =
            let case_depart = der_liste chemin in
            let case_arrivee = translate pivot (diff_case case_depart pivot)
            in
            if (est_saut config case_depart case_arrivee &&
                not (List.mem case_arrivee chemin))
            then
                Some(chemin @ [case_arrivee])
            else
                None
        in
        let sauts_possibles (config: configuration)
                            (chemin: case list): case list list =
            let (cases_plateau, _) = List.split config.cases in
            List.filter_map (saut_valides config chemin) cases_plateau
        in
        let rec loop (config: configuration) (chemins: case list list): case list list =
            let nouveaux_sauts = List.concat (
                List.rev_map (sauts_possibles config) chemins
            ) in
            match nouveaux_sauts with
            | [] -> chemins
            | _ -> chemins @ loop config nouveaux_sauts
        in
        let sauts = List.map (fun cp -> Sm(cp)) (loop config [[c]])
        in
        match sauts with
        | []  -> failwith "gros prblème à `coups_possibles`"
        | cp_absurde :: reste -> reste
    in
    depl_unit_possibles config c @
    sauts_mult_possibles (supprime_dans_config config c) c


(* Renvoie le meilleur coup possible *)
let strategie_gloutonne (config: configuration): coup =
    let delta_score (config: configuration) (action: coup): int =
        (score (appliquer_coup config action)) - (score config)
    in
    let meilleur_coup (config: configuration) (liste_coup: coup list): coup =
        let coups_et_scores = List.map (fun cp -> ((delta_score config cp), cp)) liste_coup
        in
        let coups_tries = List.rev (List.sort compare coups_et_scores)
        in
        let (_, super_coup) = List.nth coups_tries 0
        in
        super_coup
    and joueur = List.hd config.coul_joueurs
    in
    let cases_joueur =
        List.filter_map (fun (c, coul) -> if coul = joueur then Some(c) else None) config.cases
    in
    let tous_les_coups_possibles = List.concat (List.map (coups_possibles config) cases_joueur)
    in
    meilleur_coup config tous_les_coups_possibles


(* Plateau simple *)
let config_basique = remplir_init [Vert; Jaune; Rouge] 3

;;
