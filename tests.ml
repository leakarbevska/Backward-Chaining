(***  Author:  K. Lea   ***)
(***     Date: 09/03/2021      ***)
(*** Module: Intelligent Agents ***)

#use "database.ml";;
#use "chaining.ml";;


(* Fonction main appelée en premier qui montre quelques exemples et appelle la fonction principale de chaînage*)
let main () =
    print_string("Bonjour! ");
    print_string("Inserez : \n");
    print_string(" \"group\"        - pour preuve simple\n");
    print_string(" \"loop\"         - pour backtrack avec échec \n");
    print_string(" \"field\"        - pour backtrack avec solution\n");
    print_string(" \"semigroupoid\" - pour boucle\n");
    print_string(" (répondez à la deuxième question \"O\" - pour obtenir toutes les solutions) \n");
    mainChaining br bf;
    print_string("Au revoir! \n");
;;

main();;
