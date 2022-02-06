let tree_completed = Noeud(Noeud (Noeud (Vide,Int_elt 1,Vide), Int_elt 2, Noeud (Vide,Int_elt 3, Vide)), Int_elt 4, Noeud (Noeud (Vide, Int_elt 5,Vide), Int_elt 6, Noeud (Vide, Int_elt 7, Vide)));;
print_endline (string_of_bool(is_correct tree_completed));;

let tree = Noeud (Vide, Int_elt 5, Vide);;
affichage_infixe tree;;
print_endline "";;

let tree2 = add (Int_elt 4) tree;;
affichage_infixe tree2;;
print_endline "";;

let tree3 = add (Int_elt 6) tree2;;
affichage_infixe tree3;;
print_endline "";;