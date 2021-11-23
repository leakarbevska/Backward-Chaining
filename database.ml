(***  Author:  K.Lea   ***)
(***     Date: 09/03/2021      ***)
(*** Module: Intelligent Agents ***)

(*** DataBase inspiré de l'Algèbre Abstraite ***)

type rule =  Rule of (*une liste representant les regles*)
            int      (*l'index unique de la regle*)
        *   string list (*les regles contienent les premises et a la fin la consequence*)
        *   bool;;      (*un boolean spicifient si la regle est deja utilisee*)

(* ELEM_1 and ELEM_2 and ELEM_3 and...and ELEM_n-1 --> ELEM_n  *)
(* - list[list[n]] = conscequence
   - list[list[0..n-1]] = effects
   - "-NAME" == NOT NAME
*)
let br = [Rule(1, ["closed_under_*"; "identity"; "associativity"; "inverse"; "group"],false);(*R1*)
          Rule(2, ["Z"; "ring"],                        false);  (*R2*)
          Rule(3, ["C"; "ring"],                        false);  (*R3*)
          Rule(14,["magma"; "semigroupoid"       ],     false);  (*R14*)
          Rule(14,["semigroupoid"; "magma"       ],     false);  (*R14*)
          
          Rule(4, ["closed_under_*"; "closed_under_+"; "associativity"; "distributive"; "abelian_group_under_+"; "ring"],false);(*R4*) 
          Rule(5, ["commutitative_*"; "ring"; "commutative_ring"     ],false);  (*R5*)
          Rule(6, ["-commutitative_*"; "ring"; "non_commutative_ring"],false);  (*R6*)
          Rule(7, ["group"; "commutative_+"; "abelian_group_under_+"],false);  (*R7*)
          Rule(8, ["group"; "commutative_*"; "abelian_group_under_*"],false);  (*R8*)
          Rule(9, ["closed_under_+"; "closed_under_*"; "abelian_group_under_+"; "abelian_group_under_*"; "field"],false);(*R9*)
          Rule(10,["semigroup"; "identity";"monoid"],                 false);  (*R10*)
          Rule(11,["+"; "*"; "binary_operation"    ],                 false);  (*R11*)
          Rule(12,["associative";"binary_operation";"semigroup"],     false);  (*R12*)
          Rule(13,["monoid"; "commutative"; "abelian_monoid"    ],     false);  (*R13*)
          Rule(14,["magma"; "associativity"; "semigroup"       ],     false);  (*R14*)
          Rule(15,["-identity"; "associativity"; "semigroupoid"],     false);  (*R15*)
          Rule(16,["magma"; "divisibility"; "quasigroup"       ],     false);  (*R16*)
          Rule(17,["quasigroup"; "identity"; "loop" ],                false);  (*R17*)
          Rule(18,["semigroip"; "identity"; "monoid"],                false);  (*R18*)
          Rule(19,["loop"; "associativity"; "group" ],                false);  (*R19*)
          Rule(20,["monoid"; "invertibiliy"; "group"],                false);  (*R20*)
          Rule(21,["N"; "+"; "-*"; "monoid"         ],                false)];;(*R21*)
      

let bf = ["identity"; "associativity"; "closed_under_*"; "closed_under_+"; "distributive"; "inverse";
               "commutative"; "invertibiliy"; "commutative_+"; "commutative_*"];;
