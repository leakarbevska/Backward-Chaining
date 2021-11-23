(***  Author:  K.Lea   ***)
(***     Date: 09/03/2021      ***)
(*** Module: Intelligent Agents ***)
#use "database.ml";;

(*---------------------------  UTILS  -----------------------------*)

(* Retourne le dernier element de la list*)
let rec lastElement (list: 'a list) (*la liste dont nous recherchons le dernier élément*)
: 'a (*le dernier element*) 
=  
    match list with 
       | [] -> failwith "List is empty";
       | [x] -> x;
       | el::r_list -> lastElement r_list
;;

(* Supprime le dernier element de la liste *)
let rec removeLastElement (list: 'a list) (*la liste dont nous voulons supprimer le dernier element*)
: ('a list) (*la liste sans le dernier element *)
=  
    match list with 
       | [] -> failwith "List is empty";
       | [x] -> [];
       | el::r_list -> el::(removeLastElement r_list)
;;

(* Verifie si deux listes sont egaux *)
let rec areListsEqual (listA: 'a list) (*la premiere liste *)
                      (listB: 'a list) (*la deuxieme liste *)
: bool (*true si listes egaux, false sinon *)
=
match listA, listB with
    | [], [] -> true
    | [], _ -> false
    | _, [] -> false
    | a::r_listA, b::r_listB -> if (a = b) then areListsEqual r_listA r_listB  else false
;;
 


(*-----------------------------  PRINT  -------------------------------*)

(* Affichage d'une list de string*)
let rec printList(list : string list) (*la liste qui contient les elements qu'on veux afficher *)
=
    match list with 
    | [] -> [];
    | el::r_list -> print_string(el^" "); printList r_list;
;;

(* Affichage de liste de buts*)
let printNewListGoals (goals : string list) (*la liste qui contient les buts *)
=
    print_newline();
    print_string("La liste des faits à montrer devient donc :\n (");
    printList(goals);
    print_string(")");
    print_newline();
;;

(* Affichage d'une list d'un Rule*)
let rec printRule (rule: 'a list) (*la liste de Rule *)=
    match rule with
    | [] -> print_newline();
    | [consequence] -> print_string("\n  ALORS "^consequence);
    | br::r_br -> print_string(" "^br^" "); printRule r_br;
;;

(* Affichage d'un regle appliquer*)
let printRuleApplied (goal: string)      (* le but qu'on veut prouver*)
                     (rule: string list) (* la base de regles*)
                     (idx: int)          (* l'index de Rule qu'on applique*) =
    print_newline();
    print_string(goal^" peut se déduire de la règle "^(string_of_int idx)^"\n  SI");
    printRule(rule);
    print_newline();
;;  

(* Affichage d'un echec*)
let printFail (goal: string) (*le but qu'on a pas réussi à prouver *)
 = 
    print_newline();
    print_string(goal^" : ne peut pas être prouvé!");
    print_string("\n---Échec de la preuve!---\n\n");
;;

(* Affichage d'une observation*)
let printObservation (goal : string) (*le but qu'on prouve avec une observation de la base de faits*)
 =
            print_newline();
            print_string(goal^" est une observation, elle est donc vérifiée \n");
            print_newline();
;;

(*---------------------------  BACKWARD CHAINING  -----------------------------*)

(* Retourne tous les regles qui ont pour un consequent le parametre donner come "goal"*)
let rec getAllPossibleRules (goal : string)    (* le consequent qu'on veut prouver*)
                            (rules: rule list) (* la base de regles*)
: rule list =
    match rules with
    | [] -> [];
    | rule::r_rules -> let Rule(idx, rule_list, isUsed) = rule in 
                    (*si on peut prouver notre but et si la regle n'est jamais utilise *)
                    if (isUsed = false && (lastElement rule_list) = goal) 
                            (*ajout cette regle*)
                            then rule::getAllPossibleRules goal r_rules 
                            else getAllPossibleRules goal r_rules
;;

(*On aplique une règle en changeant le paramètre isUsed de cette regle dans la base de regles*)
let rec applyRule (rules : rule list) (* la base de regles *)
                  (rule_apply : string list) (*la regle qu'on veut appliquer *)
: rule list = (* la base de regles apres le changement de isUsed*)
    match rules with 
    | [] -> [];
    | rule::r_rules -> let Rule(idx, rule_list, isUsed) = rule in 
                      if (areListsEqual rule_apply rule_list = true) 
                            then Rule(idx, rule_list, true)::applyRule r_rules rule_apply
                            else rule::applyRule r_rules rule_apply
;;

let rec chainageArriere (br : rule   list) (* base de règles*)
                        (bf : string list) (* base de faits *)
                        (lb : string list) (* liste de buts*)
                        (count_fails : int)(* nombre d'eches consecutives*)
                        (get_all_solutions : bool) (*si on cherche tous les cas possibles*)
: bool (* true si la démonstration est possible, false sinon *) =
match lb with
(* la fonction travaille sur la liste des buts que l'on doit réussir à vider *)
    | [] -> 
    (* la preuve est réussie *)
        print_string("\n---Réussite de la preuve!---\n\n");
        true 
    | but::reste_lb -> (
    (* prouver le premier but, relancer sur reste de la liste des buts*)
        if (List.mem but bf = true) then (
            printObservation but;
            chainageArriere br bf reste_lb 0 get_all_solutions
        )
        else ((* essayer de le prouver avec les règles utilisables*)
            let possible_rules = getAllPossibleRules but br in 
            if (possible_rules = [] || count_fails = List.length possible_rules) 
            then (
                printFail but;
                false
            )
            else ( (*tester les règles utilisables les unes après les autres
                en relançant récursivement ; *)

                (*on prend la première règle qui peut être appliquée pour le but*)
                let Rule(idx, rule_list, isUsed) = List.nth possible_rules 0 in
                printRuleApplied but rule_list idx;

                let br_new  = applyRule br rule_list in

                (*on met à jour la base de buts en ajoutant les effets de la règle appliquée 
                et en supprimant l'objectif prouvé*)
                let lb_new  = reste_lb@(removeLastElement rule_list) in
                printNewListGoals lb_new;

                (*on appelle chainageArriere en impliquant la premier regle possible 
                et en ajoutant le but prouvé à la base des faits*)
                let outcome = (chainageArriere (br_new) (bf@[but]) lb_new 0 get_all_solutions) in
                if ( outcome = true && get_all_solutions = false) then (
                    true;
                )else (
                    (*on backtrack *)
                    print_string(">> backtrack \n");
                    (*on ajoute la règle qui nous a donné un échec à la fin de la base de règles 
                    et on incrémente le paramètre d'échec*)
                    (chainageArriere (List.append br_new [List.nth possible_rules 0]) (bf) (lb) (count_fails+1) get_all_solutions)
                )
            )
        )
    )
;; 


(*--------------------------------  MAIN  -------------------------------*)
(*Fonction principale du fichier qui lit les entrées de l'utilisateur et appelle la fonction de chaînageArriere.*)
let rec mainChaining (br : rule list)  (*la base de regles*)
                     (bf : string list) (*la base de faits*)
    =
    print_string("Quel fait voulez-vous montrer? \n");
    let lb = read_line() in

    print_string("Désirez-vous observer tous les cas possibles? (O/N) : \n");
    let ans_allSolutions = read_line() in

    if (ans_allSolutions = "O") then (chainageArriere br bf [lb] 0 true)
                                else (chainageArriere br bf [lb] 0 false);


    print_string("Désirez-vous essayer une démonstration différente (O/N) : \n");
    let ans = read_line() in
    if ( ans = "O") then mainChaining br bf;
;;
