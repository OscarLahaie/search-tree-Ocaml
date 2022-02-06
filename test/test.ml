open SearchTree;;

let tree_completed = Noeud(Noeud (Noeud (Vide,Int_elt 1,Vide), Int_elt 2, Noeud (Vide,Int_elt 3, Vide)), Int_elt 4, Noeud (Noeud (Vide, Int_elt 5,Vide), Int_elt 6, Noeud (Vide, Int_elt 7, Vide)));;

let tree = Noeud (Vide, Int_elt 5, Vide);;

let tree2 = add (Int_elt 4) tree;;

let tree3 = add (Int_elt 6) tree2;;