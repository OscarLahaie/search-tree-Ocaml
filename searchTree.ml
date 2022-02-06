(** Data stored in the tree *)
type tree_data = 
  | Int_elt of int
  | Float_elt of float
  | String_elt of string
  | Char_elt of char
  | Couple_elt of tree_data * tree_data
  | None_elt
;;

(** Data model to store the tree *)
type 'tree_data abr =
  | Vide
  | Noeud of 'tree_data abr * 'tree_data * 'tree_data abr
;;

(** Compare the data of the tree. Return an integer which symbolise the difference between two numbers. *)
let rec compare_data a b =
  match a,b with
  | Int_elt a, Int_elt b -> a - b
  | Float_elt a, Float_elt b -> int_of_float(ceil(Float.abs(a-.b))) * int_of_float(Float.abs(a-. b)/. (a-.b))
  | String_elt a, String_elt b -> String.compare a b
  | Char_elt a, Char_elt b -> Char.compare a b
  | Couple_elt (a,_), Couple_elt (b,_) -> compare_data a b
  | _ -> failwith("Invalid comparaison")
;;

  (** Print data of a tree element. *)
  let rec print_data data =
    match data with
  | Int_elt i -> print_int i
  | Float_elt f -> print_float f
  | String_elt s -> print_string s
  | Char_elt c -> print_char c
  | Couple_elt (a,b) -> print_string "("; print_data a; print_string", "; print_data b; print_string ")"
  | None_elt -> print_string " "
;;

(** Print the tree. The order :  Left child, Node value, Right child. *)
let rec affichage_infixe a =
  match a with
  | Vide -> ()
  | Noeud (cl, e, cr) ->
    affichage_infixe cl;
    print_char ' ';
    print_data e;
    print_char ' ';
    affichage_infixe cr
;;

(** Print the tree. The order :  Node value, Left child, Right child. *)
let rec affichage_prefixe a =
  match a with
  | Vide -> ()
  | Noeud (cl, e, cr) ->
    print_data e;
    print_char ' ';
    affichage_prefixe cl;
    print_char ' ';
    affichage_prefixe cr;
  ;;
  
  (** Print the tree. The order : Left child, Right child, Node value. *)
let rec affichage_suffixe a =
  match a with
  | Vide -> ()
  | Noeud (cl, e, cr) ->
    affichage_suffixe cl;
    print_char ' ';
    affichage_suffixe cr;
    print_char ' ';
    print_data e;
  ;;
  
  
  (** Verify if the tree is valid. Scan the elements of the tree to find if there are in the right order. *)   
let is_correct tree =
  let rec aux_correct tree =
    match tree with
    | Noeud (Vide, e, Vide) -> true, e, e
    | Noeud (cl, e, Vide) ->  
      let tree_l, mini_l, maxi_l = aux_correct cl in (tree_l && compare_data maxi_l e <= 0, mini_l, e)
    | Noeud (Vide, e, cr ) -> 
      let tree_r, mini_r, maxi_r = aux_correct cr in (tree_r && compare_data e mini_r <= 0, e, maxi_r)
    | Noeud (cl, e, cr) -> 
      let tree_l, mini_l, maxi_l = aux_correct cl in
      let tree_r, mini_r, maxi_r = aux_correct cr in
      (tree_r && tree_l && compare_data maxi_l e <= 0 && compare_data e mini_r <= 0, mini_l, maxi_r)
    | Vide -> failwith ("Tree empty but it is impossible")
  in tree = Vide || let res, _, _ = aux_correct tree in res 
;;

(** Return the number of node. *)
let rec size tree =
  match tree with
  | Vide -> 0
  | Noeud (cl, _, cr) -> 1 + size cl + size cr
;;

(** Check if a tree have the searched key*)
let rec mem key tree =
  match tree with
  | Vide -> false
  | Noeud (_, e, _) when compare_data key e = 0 -> true
  | Noeud (cl, e, _) when compare_data key e < 0 -> mem key cl
  | Noeud (_, _, cr) -> mem key cr
;;

(** Add at the right place the wanted element. *)
let rec add elt tree =
  match tree with
  | Vide -> Noeud (Vide, elt, Vide)
  | Noeud (_, e, _) when compare_data elt e = 0 -> tree
  | Noeud (cl, e, cr) when compare_data elt e < 0 -> Noeud (add elt cl, e, cr)
  | Noeud (cl, e, cr) -> Noeud (cl, e, add elt cr)
;;

(** Return the minimal element of the tree. Send an error if the tree is empty *)
let rec min_elt tree =
  match tree with
  | Vide -> failwith "arbre vide"
  | Noeud (Vide, e, _) -> e
  | Noeud (cl, _, _) -> min_elt cl
;;

(** Return the minimal element of the tree. The optionnal version of min_elt *)
let rec min_elt_opt tree =
  match tree with
  | Vide -> None
  | Noeud (Vide, e, _) -> Some e
  | Noeud (cl, _, _) -> min_elt_opt cl
;;

(** Return the maximal element of the tree. Send an error if the tree is empty *)
let rec max_elt tree =
  match tree with
  | Vide -> failwith "arbre vide"
  | Noeud (_, e, Vide) -> e
  | Noeud (_, _, cr) -> max_elt cr
;;

(** Return the maximal element of the tree. The optionnal version of max_elt *)
let rec max_elt_opt tree =
  match tree with
  | Vide -> None
  | Noeud (_, e, Vide) -> Some e
  | Noeud (_, _, cr) -> max_elt_opt cr
;;


(** Return the tree without the element of the selected key *)
let rec delete key tree =
  let rec delete_min_elt tree =
    match tree with
    | Vide -> failwith "empty tree"
    | Noeud (Vide, e, cr) -> (e, cr)
    | Noeud (cr, e, cl) ->
       let x, cr_del = delete_min_elt cr in
       (x, Noeud (cr_del, e, cl))
    in
    match tree with
    | Vide -> tree
    | Noeud (cl, e, Vide) when compare_data key e = 0 -> cl
    | Noeud (cl, e, cr) when compare_data key e = 0 ->
        let m, cr_del = delete_min_elt cr in
        Noeud (cl, m, cr_del)
    | Noeud (cl, e, cr) when compare_data key e < 0 ->
        Noeud (delete key cl, e, cr)
    | Noeud (cl, e, cr) ->
        Noeud (cl, e, delete key cr)
;;