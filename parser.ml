open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)
let rec parse_expr toks = 
  
  match lookahead toks with

  | Some Tok_Let -> parse_let toks

  | Some Tok_If -> parse_if toks

  | Some Tok_Fun -> parse_fun toks

  | _ -> parse_or toks

and parse_let toks = 

    let tok = match_token toks Tok_Let in

    match (lookahead tok, lookahead_many tok 1, lookahead_many tok 2) with 

    | (Some Tok_Rec, Some Tok_ID id, Some Tok_Equal) -> 
      let tok' = match_token tok Tok_Rec in 
      let tok'' = match_token tok' (Tok_ID id) in
      let tok''' = match_token tok'' Tok_Equal in
      let (t1, expr) = parse_expr tok''' in 
      let tok'''' = match_token t1 Tok_In in
      let (t2, expr') = parse_expr tok'''' in 
      (t2, Let(id, true, expr, expr'))

    | (Some (Tok_ID id), Some (Tok_Equal), _) -> 
      let t = match_token tok (Tok_ID(id)) in
      let t' = match_token t Tok_Equal in        
      let (t1, expr) = parse_expr t' in 
      let t'' = match_token t1 Tok_In in
      let (t2, expr') = parse_expr t'' in 
      (t2, Let(id, false, expr, expr'))

    | _ -> raise (InvalidInputException("Parse Let Error"))

and parse_if toks = 

  let first_tok = match_token toks Tok_If in
  let (second_tok, first) = parse_expr first_tok in
  let next = lookahead second_tok in

  if next = Some (Tok_Then) then

    let after = match_token second_tok Tok_Then in
    let (third_token, second) = parse_expr after in
    let fourth_tok = lookahead third_token in

    if fourth_tok = Some Tok_Else then

      let else_token = match_token third_token Tok_Else in
      let (fifth_tok, last) = parse_expr else_token in
      (fifth_tok, If(first, second, last))

    else raise (InvalidInputException "Parse If Failed")

  else raise (InvalidInputException "Parse If Failed")

and parse_fun toks =
 
  let toks = match_token toks Tok_Fun in
  
  match lookahead toks with
  
    | Some Tok_ID x ->
      let tok = match_token toks (Tok_ID x) in
      let first_tok = match_token tok Tok_Arrow in
      let (second_tok, exp) = parse_expr first_tok in
      (second_tok, Fun(x, exp))
  
    | _ -> raise (InvalidInputException("Parse Function Error"))

and parse_or toks = 

  let (tok, expr) = parse_and toks in

  match lookahead tok with

  | Some (Tok_Or) -> 
    let first_tok = match_token tok Tok_Or in
    let (second_tok, expr') = parse_or first_tok in 
    (second_tok, Binop(Or, expr, expr'))

  | _ -> tok, expr

and parse_and toks = 

  let (tok, expr) = parse_equal_expr toks in

  match lookahead tok with 

  | Some Tok_And -> 
    let first_tok = match_token tok Tok_And in
    let (second_tok, expr') = parse_and first_tok in 
    (second_tok, Binop(And, expr, expr'))

  | _ -> tok, expr

and parse_equal_expr toks = 

  let (tok, expr) = parse_rel_expr toks in

  match lookahead tok with

  | Some Tok_NotEqual -> 
    let first_tok = match_token tok Tok_NotEqual in
    let (second_tok, expr') = parse_equal_expr first_tok in
    (second_tok, Binop(NotEqual, expr, expr'))

  | Some Tok_Equal -> 
    let first_tok = match_token tok Tok_Equal in
    let (second_tok, expr') = parse_equal_expr first_tok in
    (second_tok, Binop(Equal, expr, expr'))

  | _ -> tok, expr

and parse_rel_expr toks = 

  let (tok, expr) = parse_add_expr toks in 

  match lookahead tok with 

  | Some Tok_Less -> 
    let first_tok = match_token tok Tok_Less in
    let (second_tok, expr') = parse_rel_expr first_tok in
    (second_tok, Binop(Less, expr, expr'))

  | Some Tok_Greater -> 
    let first_tok = match_token tok Tok_Greater in
    let (second_tok, expr') = parse_rel_expr first_tok in
    (second_tok, Binop(Greater, expr, expr'))

  | Some Tok_GreaterEqual -> 
    let first_tok = match_token tok Tok_GreaterEqual in
    let (second_tok, expr') = parse_rel_expr first_tok in
    (second_tok, Binop(GreaterEqual, expr, expr'))

  | Some Tok_LessEqual ->
    let first_tok = match_token tok Tok_LessEqual in
    let (second_tok, expr'') = parse_rel_expr first_tok in
    (second_tok, Binop(LessEqual, expr, expr''))

  | _ -> tok, expr

and parse_add_expr toks =

  let (tok, expr) = parse_mult_expr toks in

  match lookahead tok with 

  | Some Tok_Add ->
    let first_tok = match_token tok Tok_Add in
    let (second_tok, expr') = parse_add_expr first_tok in
    (second_tok, Binop(Add, expr, expr'))

  | Some Tok_Sub ->
    let first_tok = match_token tok Tok_Sub in
    let (second_tok, expr') = parse_add_expr first_tok in
    (second_tok, Binop(Sub, expr, expr'))

  | _ -> tok, expr

and parse_mult_expr toks =

  let (tok, expr) = parse_cat_expr toks in

  match lookahead tok with

  | Some Tok_Mult -> 
    let first_tok = match_token tok Tok_Mult in
    let (second_tok, expr') = parse_mult_expr first_tok in
    (second_tok, Binop(Mult, expr, expr'))

  | Some Tok_Div -> 
    let first_tok = match_token tok Tok_Div in
    let (second_tok, expr') = parse_mult_expr first_tok in
    (second_tok, Binop(Div, expr, expr'))

  | _ -> tok, expr

and parse_cat_expr toks =

  let (tok, expr) = parse_unary_expr toks in

  match lookahead tok with

  | Some Tok_Concat -> 
    let first_tok = match_token tok Tok_Concat in
    let (second_tok, expr') = parse_cat_expr first_tok in
    (second_tok, Binop(Concat, expr, expr'))
  | _ -> tok, expr

and parse_unary_expr toks =

  match lookahead toks with

  | Some Tok_Not ->
    let tok = match_token toks Tok_Not in
    let (first_tok, expr) = parse_unary_expr tok in
    (first_tok, Not expr)

  | _ -> parse_fun_expr toks

and parse_fun_expr toks =

  let (tok, expr) = parse_primary_expr toks in

  match lookahead tok with
  
  | Some (Tok_ID id) -> 
    let (first_tok, expr') = parse_primary_expr tok in 
    (first_tok, FunctionCall(expr, expr'))

  | Some (Tok_Bool b) -> 
    let (first_tok, expr') = parse_primary_expr tok in
    (first_tok, FunctionCall(expr, expr'))

  | Some (Tok_String s) -> 
    let (first_tok, expr') = parse_primary_expr tok in
    (first_tok, FunctionCall(expr, expr'))

  | Some (Tok_Int i) -> 
    let (first_tok, expr') = parse_primary_expr tok in
    (first_tok, FunctionCall(expr, expr'))

  | Some (Tok_LParen) -> 
    let (first_tok, expr') = parse_primary_expr tok in
    (first_tok, FunctionCall(expr, expr'))

  | _ -> tok, expr

and parse_primary_expr toks = 

  match lookahead toks with 

  | Some (Tok_ID id) ->
    let first_tok = match_token toks (Tok_ID id) in 
    (first_tok, ID id)

  | Some (Tok_Bool boolean) -> 
    let first_tok = match_token toks (Tok_Bool boolean) in 
    (first_tok, Value(Bool boolean))

  | Some (Tok_String(s)) -> 
    let first_tok = match_token toks (Tok_String(s)) in
    (first_tok, Value(String(s)))

  | Some (Tok_Int(i)) -> 
    let first_tok = match_token toks (Tok_Int(i)) in
    (first_tok, Value(Int(i)))

  | Some (Tok_LParen) -> 
    let first_tok = match_token toks (Tok_LParen) in
    let (second_tok, expr) = parse_expr first_tok in 
    let third_tok = match_token second_tok (Tok_RParen) in 
    (third_tok, expr)

  | _ -> raise(InvalidInputException("Primary Expression Error"))

let rec parse_mutop toks =

  match lookahead toks with

  | Some Tok_Def ->

    (let tok_list = match_token toks (Tok_Def) in
    
     match lookahead tok_list with 

     | Some Tok_ID(id) -> 
       let first_tok = match_token tok_list (Tok_ID(id)) in
       let second_tok = match_token first_tok (Tok_Equal) in 
       let (third_tok, expr) = parse_expr second_tok in
       let fourth_tok = match_token third_tok (Tok_DoubleSemi) in
       (fourth_tok, Def(id, expr))

    | _ -> raise (InvalidInputException("Mutop Parsing Error")))

  | Some Tok_DoubleSemi -> 
    let tok_list = match_token toks (Tok_DoubleSemi) in
    (tok_list, NoOp)

  | _ -> 
    let (tok, expr) = parse_expr toks in 
    let second_tok = match_token tok (Tok_DoubleSemi) in
    (second_tok, Expr expr)