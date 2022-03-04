(** Data stored in the tree *)
type tree_data =
  | Int_elt of int
  | Float_elt of float
  | String_elt of string
  | Char_elt of char
  | Couple_elt of tree_data * tree_data
  | None_elt

(** Data model to store the tree *)
type abr =
  | Empty
  | Node of abr * tree_data * abr

(** Compare the data of the tree. Return an integer which symbolise the difference between two numbers. *)
let rec compare_data a b =
  match a, b with
  | Int_elt a, Int_elt b -> a - b
  | Float_elt a, Float_elt b ->
    int_of_float (ceil (Float.abs (a -. b)))
    * int_of_float (Float.abs (a -. b) /. (a -. b))
  | String_elt a, String_elt b -> String.compare a b
  | Char_elt a, Char_elt b -> Char.compare a b
  | Couple_elt (a, _), Couple_elt (b, _) -> compare_data a b
  | _ -> failwith "Invalid comparaison"
;;

(** Print data of a tree element. *)
let rec print_data data =
  match data with
  | Int_elt i -> print_int i
  | Float_elt f -> print_float f
  | String_elt s -> print_string s
  | Char_elt c -> print_char c
  | Couple_elt (a, b) ->
    print_string "(";
    print_data a;
    print_string ", ";
    print_data b;
    print_string ")"
  | None_elt -> print_string " "
;;

(** Print the tree. The order :  Left child, Node value, Right child. *)
let rec affichage_infixe a =
  match a with
  | Empty -> ()
  | Node (cl, e, cr) ->
    affichage_infixe cl;
    print_char ' ';
    print_data e;
    print_char ' ';
    affichage_infixe cr
;;

(** Print the tree. The order :  Node value, Left child, Right child. *)
let rec affichage_prefixe a =
  match a with
  | Empty -> ()
  | Node (cl, e, cr) ->
    print_data e;
    print_char ' ';
    affichage_prefixe cl;
    print_char ' ';
    affichage_prefixe cr
;;

(** Print the tree. The order : Left child, Right child, Node value. *)
let rec affichage_suffixe a =
  match a with
  | Empty -> ()
  | Node (cl, e, cr) ->
    affichage_suffixe cl;
    print_char ' ';
    affichage_suffixe cr;
    print_char ' ';
    print_data e
;;

(** Return the height of the tree *)
let rec height tree =
  match tree with
  | Empty -> 0
  | Node (cl, _, cr) -> 1 + max (height cl) (height cr)

(** ASCII ART tree printing *)

(** Verify if the tree is valid. Scan the elements of the tree to find if there are in the right order. *)
let is_correct tree =
  let rec aux_correct tree =
    match tree with
    | Node (Empty, e, Empty) -> true, e, e
    | Node (cl, e, Empty) ->
      let tree_l, mini_l, maxi_l = aux_correct cl in
      tree_l && compare_data maxi_l e <= 0, mini_l, e
    | Node (Empty, e, cr) ->
      let tree_r, mini_r, maxi_r = aux_correct cr in
      tree_r && compare_data e mini_r <= 0, e, maxi_r
    | Node (cl, e, cr) ->
      let tree_l, mini_l, maxi_l = aux_correct cl in
      let tree_r, mini_r, maxi_r = aux_correct cr in
      ( tree_r && tree_l && compare_data maxi_l e <= 0 && compare_data e mini_r <= 0
      , mini_l
      , maxi_r )
    | Empty -> failwith "Tree empty but it is impossible"
  in
  tree = Empty
  ||
  let res, _, _ = aux_correct tree in
  res
;;

(** Return the number of node. *)
let rec size tree =
  match tree with
  | Empty -> 0
  | Node (cl, _, cr) -> 1 + size cl + size cr
;;

(** Check if a tree have the searched key*)
let rec mem key tree =
  match tree with
  | Empty -> false
  | Node (_, e, _) when compare_data key e = 0 -> true
  | Node (cl, e, _) when compare_data key e < 0 -> mem key cl
  | Node (_, _, cr) -> mem key cr
;;

(** Add at the right place the wanted element. *)
let rec add elt tree =
  match tree with
  | Empty -> Node (Empty, elt, Empty)
  | Node (_, e, _) when compare_data elt e = 0 -> tree
  | Node (cl, e, cr) when compare_data elt e < 0 -> Node (add elt cl, e, cr)
  | Node (cl, e, cr) -> Node (cl, e, add elt cr)
;;

(** Return an optimized version of the given tree *)

(** Return the minimal element of the tree. Send an error if the tree is empty *)
let rec min_elt tree =
  match tree with
  | Empty -> failwith "arbre Empty"
  | Node (Empty, e, _) -> e
  | Node (cl, _, _) -> min_elt cl
;;

(** Return the minimal element of the tree. The optionnal version of min_elt *)
let rec min_elt_opt tree =
  match tree with
  | Empty -> None
  | Node (Empty, e, _) -> Some e
  | Node (cl, _, _) -> min_elt_opt cl
;;

(** Return the maximal element of the tree. Send an error if the tree is empty *)
let rec max_elt tree =
  match tree with
  | Empty -> failwith "arbre Empty"
  | Node (_, e, Empty) -> e
  | Node (_, _, cr) -> max_elt cr
;;

(** Return the maximal element of the tree. The optionnal version of max_elt *)
let rec max_elt_opt tree =
  match tree with
  | Empty -> None
  | Node (_, e, Empty) -> Some e
  | Node (_, _, cr) -> max_elt_opt cr
;;

(** Return the tree without the element of the selected key *)
let rec delete key tree =
  let rec delete_min_elt tree =
    match tree with
    | Empty -> failwith "empty tree"
    | Node (Empty, e, cr) -> e, cr
    | Node (cr, e, cl) ->
      let x, cr_del = delete_min_elt cr in
      x, Node (cr_del, e, cl)
  in
  match tree with
  | Empty -> tree
  | Node (cl, e, Empty) when compare_data key e = 0 -> cl
  | Node (cl, e, cr) when compare_data key e = 0 ->
    let m, cr_del = delete_min_elt cr in
    Node (cl, m, cr_del)
  | Node (cl, e, cr) when compare_data key e < 0 -> Node (delete key cl, e, cr)
  | Node (cl, e, cr) -> Node (cl, e, delete key cr)
;;
