let parse_ID toks = 
  match (lookahead toks) with
    | Some Tok_ID k -> let tokens = match_token toks (Tok_ID k) in
                    (tokens, k)

    | _ -> raise (InvalidInputException "failed")


let rec parse_expr toks = 
  match (lookahead toks) with
    | Some Tok_Let -> let (tokens, expr) = parse_LetExp toks in
                    (tokens, expr)

    | Some Tok_If -> let (tokens, expr) = parse_IfExp toks in
                    (tokens, expr)

    | Some Tok_Fun -> let (tokens, expr) = parse_FunExp toks in
                    (tokens, expr)

    | _ -> parse_OrExp toks


and parse_LetExp toks = 
  let tokens1 = match_token toks Tok_Let in               
  match (lookahead tokens1) with
  | Some Tok_Rec -> let tokens2 = match_token tokens1 Tok_Rec in
                    let (tokens3, expr_ID) = parse_ID tokens2 in
                      let (tokens4, expr1) = parse_expr (match_token tokens3 Tok_Equal) in
                        let (tokens5, expr2) = parse_expr (match_token tokens4 Tok_In) in
                            (tokens5, Let (expr_ID, true, expr1, expr2))

  | _ -> let (tokens2, expr_ID) = parse_ID tokens1 in
          let (tokens3, expr1) = parse_expr (match_token tokens2 Tok_Equal) in
            let (tokens4, expr2) = parse_expr (match_token tokens3 Tok_In) in
                (tokens4, Let (expr_ID, false, expr1, expr2))


and parse_FunExp toks = 
  let tokens1 = match_token toks Tok_Fun in
    let (tokens2, expr_ID) = parse_ID tokens1 in
      let (tokens3, expr1) = parse_expr (match_token tokens2 Tok_Arrow) in
          (tokens3, Fun (expr_ID, expr1))


and parse_IfExp toks = 
  let (tokens2, expr1) = parse_expr (match_token toks Tok_If) in
    let (tokens3, expr2) = parse_expr (match_token tokens2 Tok_Then) in
      let (tokens4, expr3) = parse_expr (match_token tokens3 Tok_Else) in
          (tokens4, If (expr1, expr2, expr3))


and parse_AndExp toks = 
  let (toks_after_parse_EqualityExp, expr) = parse_EqualityExp toks in
    match (lookahead toks_after_parse_EqualityExp) with
    | Some Tok_And -> let tokens2 = match_token toks_after_parse_EqualityExp Tok_And in
                  let (tokens3, expr_after_parse_AndExp) = parse_AndExp tokens2 in
                  (tokens3, Binop (And, expr, expr_after_parse_AndExp))

    | _ -> (toks_after_parse_EqualityExp, expr)


and parse_OrExp toks = 
  let (toks_after_parse_AndExp, expr) = parse_AndExp toks in
      match (lookahead toks_after_parse_AndExp) with
      | Some Tok_Or -> let tokens2 = match_token toks_after_parse_AndExp Tok_Or in
                    let (tokens3, expr_after_parse_OrExp) = parse_OrExp tokens2 in
                    (tokens3, Binop (Or, expr, expr_after_parse_OrExp))

      | _ -> (toks_after_parse_AndExp, expr)


