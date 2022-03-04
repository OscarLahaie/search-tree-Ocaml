open SearchTree;;

let tree_completed = Node(Node (Node (Empty,Int_elt 1,Empty), Int_elt 2, Node (Empty,Int_elt 3, Empty)), Int_elt 4, Node (Node (Empty, Int_elt 5,Empty), Int_elt 6, Node (Empty, Int_elt 7, Empty)));;
print_endline (string_of_bool(is_correct tree_completed));;

let tree = Node (Empty, Int_elt 5, Empty);;
affichage_infixe tree;;
print_endline "";;

let tree2 = add (Int_elt 4) tree;;
affichage_infixe tree2;;
print_endline "";;

let tree3 = add (Int_elt 6) tree2;;
affichage_infixe tree3;;
print_endline "";;

print_int (height tree_completed);;
print_endline "";;

print_int (height tree);;
print_endline "";;