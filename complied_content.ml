(* pc.ml
 * The new implementation of the parsing-combinators package for ocaml
 *
 * Prorammer: Mayer Goldberg, 2021
 *)

(* general list-processing procedures *)

let list_of_string string =
  let rec run i s =
    if i < 0 then s
    else run (i - 1) (string.[i] :: s) in
  run (String.length string - 1) [];;

let string_of_list s =
  List.fold_left
    (fun str ch -> str ^ (String.make 1 ch))
    ""
    s;;

let rec ormap f s =
  match s with
  | [] -> false
  | car :: cdr -> (f car) || (ormap f cdr);;

let rec andmap f s =
  match s with
  | [] -> true
  | car :: cdr -> (f car) && (andmap f cdr);;	  

module PC = struct

  type 'a parsing_result = {
      index_from : int;
      index_to : int;
      found : 'a
    };;

  type 'a parser = string -> int -> 'a parsing_result;;

  (* the parsing combinators defined here *)
  
  exception X_not_yet_implemented;;

  exception X_no_match;;

  let const pred =
    ((fun str index ->
      if (index < String.length str) && (pred str.[index])
      then {
          index_from = index;
          index_to = index + 1;
          found = str.[index]
        }
      else raise X_no_match) : 'a parser);;

  let caten (nt_1 : 'a parser) (nt_2 : 'b parser) =
    ((fun str index ->
      let {index_from = index_from_1;
           index_to = index_to_1;
           found = e_1} = (nt_1 str index) in
      let {index_from = index_from_2;
           index_to = index_to_2;
           found = e_2} = (nt_2 str index_to_1) in
      {index_from = index_from_1;
       index_to = index_to_2;
       found = (e_1, e_2)}) : (('a * 'b) parser));;

  let pack (nt : 'a parser) (f : 'a -> 'b) =
    ((fun str index -> 
      let {index_from; index_to; found} = (nt str index) in
      {index_from; index_to; found = (f found)})
     : 'b parser);;

  let nt_epsilon =
    ((fun str index ->
      {index_from = index;
       index_to = index;
       found = []}) : 'a parser);;

  let caten_list nts =
    List.fold_right
      (fun nt1 nt2 ->
        pack (caten nt1 nt2)
	  (fun (e, es) -> (e :: es)))
      nts
      nt_epsilon;;

  let disj (nt1 : 'a parser) (nt2 : 'a parser) =
    ((fun str index -> 
      try (nt1 str index)
      with X_no_match -> (nt2 str index)) : 'a parser);;

  let nt_none = ((fun _str _index -> raise X_no_match) : 'a parser);;
  
  let disj_list nts = List.fold_right disj nts nt_none;;

  let delayed (thunk : unit -> 'a parser) =
    ((fun str index -> thunk() str index) : 'a parser);;

  let nt_end_of_input str index = 
    if (index < String.length str)
    then raise X_no_match
    else {index_from = index; index_to = index; found = []};;

  let rec star (nt : 'a parser) =
    ((fun str index ->
      try let {index_from = index_from_1;
               index_to = index_to_1;
               found = e} = (nt str index) in
          let {index_from = index_from_rest;
               index_to = index_to_rest;
               found = es} = (star nt str index_to_1) in
          {index_from = index_from_1;
           index_to = index_to_rest;
           found = (e :: es)}
      with X_no_match -> {index_from = index; index_to = index; found = []})
     : 'a list parser);;

  let plus nt =
    pack (caten nt (star nt))
      (fun (e, es) -> (e :: es));;

  let rec power nt n =
    if n = 0 then nt_epsilon
    else pack(caten nt (power nt (n - 1)))
           (fun (e, es) -> e :: es);;    

  let at_least nt n =
    pack (caten (power nt n) (star nt))
      (fun (es_1, es_2) -> es_1 @ es_2);;

  let only_if (nt : 'a parser) pred =
    ((fun str index ->
      let ({index_from; index_to; found} as result) = (nt str index) in
      if (pred found) then result
      else raise X_no_match) : 'a parser);;

  let maybe (nt : 'a parser) =
    ((fun str index ->
      try let {index_from; index_to; found} = (nt str index) in
          {index_from; index_to; found = Some(found)}
      with X_no_match ->
        {index_from = index; index_to = index; found = None})
     : 'a option parser);;  

  let diff nt1 nt2 =
    ((fun str index ->
      match (maybe nt1 str index) with
      | {index_from; index_to; found = None} -> raise X_no_match
      | {index_from; index_to; found = Some(e)} ->
         match (maybe nt2 str index) with
         | {index_from = _; index_to = _; found = None} ->
            {index_from; index_to; found = e}
         | _ -> raise X_no_match) : 'a parser);;

  let followed_by (nt1 : 'a parser) (nt2 : 'b parser) =
    ((fun str index -> 
      let ({index_from; index_to; found} as result) = (nt1 str index) in
      let _ = (nt2 str index_to) in
      result) : 'a parser);;

  let not_followed_by (nt1 : 'a parser) (nt2 : 'b parser) =
    ((fun str index ->
      match (let ({index_from; index_to; found} as result) = (nt1 str index) in
	     try let _ = (nt2 str index_to) in
	         None
	     with X_no_match -> (Some(result))) with
      | None -> raise X_no_match
      | Some(result) -> result) : 'a parser);;
  
  (* useful general parsers for working with text *)

  let make_char equal ch1 = const (fun ch2 -> equal ch1 ch2);;

  let char = make_char (fun ch1 ch2 -> ch1 = ch2);;

  let char_ci =
    make_char (fun ch1 ch2 ->
	(Char.lowercase_ascii ch1) =
	  (Char.lowercase_ascii ch2));;

  let make_word char str = 
    List.fold_right
      (fun nt1 nt2 -> pack (caten nt1 nt2) (fun (a, b) -> a :: b))
      (List.map char (list_of_string str))
      nt_epsilon;;

  let word = make_word char;;

  let word_ci = make_word char_ci;;

  let make_one_of char str =
    List.fold_right
      disj
      (List.map char (list_of_string str))
      nt_none;;

  let one_of = make_one_of char;;

  let one_of_ci = make_one_of char_ci;;

  let nt_whitespace = const (fun ch -> ch <= ' ');;

  let make_range leq ch1 ch2 =
    const (fun ch -> (leq ch1 ch) && (leq ch ch2));;

  let range = make_range (fun ch1 ch2 -> ch1 <= ch2);;

  let range_ci =
    make_range (fun ch1 ch2 ->
	(Char.lowercase_ascii ch1) <=
	  (Char.lowercase_ascii ch2));;

  let nt_any = ((fun str index -> const (fun ch -> true) str index) : 'a parser);;

  let trace_pc desc (nt : 'a parser) =
    ((fun str index ->
      try let ({index_from; index_to; found} as value) = (nt str index)
          in
          (Printf.printf ";;; %s matched from char %d to char %d, leaving %d chars unread\n"
	     desc
	     index_from index_to
             ((String.length str) - index_to) ;
           value)
      with X_no_match ->
        (Printf.printf ";;; %s failed\n"
	   desc ;
         raise X_no_match)) : 'a parser);;

  (* testing the parsers *)

  let test_string (nt : 'a parser) str index =
    nt str index;;

  let search_forward (nt : 'a parser) str =
    let limit = String.length str in
    let rec run i =
      if (i < limit)
      then (match (maybe nt str i) with
            | {index_from; index_to; found = None} -> run (i + 1)
            | {index_from; index_to; found = Some(e)} ->
               {index_from; index_to; found = e})
      else raise X_no_match in
    run 0;; 

  let search_forward_all (nt : 'a parser) str =
    let limit = String.length str in
    let rec run i = 
      if (i < limit)
      then (match (maybe nt str i) with
            | {index_from; index_to; found = None} -> run (i + 1)
            | {index_from; index_to; found = Some(e)} ->
               {index_from; index_to; found = e} :: (run (i + 1)))
      else [] in
    run 0;;

  let search_backward (nt : 'a parser) str =
    let rec run i =
      if (-1 < i)
      then (match (maybe nt str i) with
            | {index_from; index_to; found = None} -> run (i - 1)
            | {index_from; index_to; found = Some(e)} ->
               {index_from; index_to; found = e})
      else raise X_no_match in
    run (String.length str - 1);; 

  let search_backward_all (nt : 'a parser) str =
    let limit = String.length str in
    let rec run i = 
      if (-1 < i)
      then (match (maybe nt str i) with
            | {index_from; index_to; found = None} -> run (i - 1)
            | {index_from; index_to; found = Some(e)} ->
               {index_from; index_to; found = e} :: (run (i - 1)))
      else [] in
    run (limit - 1);;

end;; (* end of struct PC *)

(* end-of-input *)


exception X_not_yet_implemented;;
exception X_this_should_not_happen of string;;

let rec is_member a = function
  | [] -> false
  | a' :: s -> (a = a') || (is_member a s);;

let rec gcd a b =
  match (a, b) with
  | (0, b) -> b
  | (a, 0) -> a
  | (a, b) -> gcd b (a mod b);;

type scm_number =
  | ScmRational of (int * int)
  | ScmReal of float;;

type sexpr =
  | ScmVoid
  | ScmNil
  | ScmBoolean of bool
  | ScmChar of char
  | ScmString of string
  | ScmSymbol of string
  | ScmNumber of scm_number
  | ScmVector of (sexpr list)
  | ScmPair of (sexpr * sexpr);;

module type READER = sig
  val nt_sexpr : sexpr PC.parser
  val print_sexpr : out_channel -> sexpr -> unit
  val print_sexprs : out_channel -> sexpr list -> unit
  val sprint_sexpr : 'a -> sexpr -> string
  val sprint_sexprs : 'a -> sexpr list -> string
  val scheme_sexpr_list_of_sexpr_list : sexpr list -> sexpr
end;; (* end of READER signature *)

module Reader = struct
  open PC;;

  type string_part =
    | Static of string
    | Dynamic of sexpr;;

  let unitify nt = pack nt (fun _ -> ());;
  let make_maybe nt none_value =
    pack (maybe nt)
      (function
       | None -> none_value
       | Some(x) -> x);;
  let ascii_char code = char (char_of_int code);;
  let complement (nt: 'a parser) = diff nt_any nt
  let word_of_char (ch: char) = pack (char ch) (fun c -> string_of_list [c])
  let non_escaped (c: char) =
    ((fun str index ->
        if index < 2 || str.[index-2] != '#' || str.[index-1] != '\\' then (word_of_char c) str index
        else raise X_no_match
     ): string parser);;



  let rec nt_whitespace str =
    const (fun ch -> ch <= ' ') str
  and nt_end_of_line_or_file str =
    let nt1 = unitify (char '\n') in
    let nt2 = unitify nt_end_of_input in
    let nt1 = disj nt1 nt2 in
    nt1 str
  and nt_line_comment str =
    let nt1 = char ';' in
    let nt2 = diff nt_any nt_end_of_line_or_file in
    let nt2 = star nt2 in
    let nt1 = caten nt1 nt2 in
    let nt1 = caten nt1 nt_end_of_line_or_file in
    let nt1 = unitify nt1 in
    nt1 str
  and nt_sexp_comment_symbol str = (pack (word "#;") (fun _ -> ())) str
  and nt_sexpr_comment str = (pack (caten nt_sexp_comment_symbol nt_sexpr) (fun _ -> ())) str

  and nt_left_curly_bracket str = (non_escaped '{') str
  and nt_right_curly_bracket str = (non_escaped '}') str
  and nt_legit_paired_comment_chars str = (diff (complement nt_left_curly_bracket) nt_right_curly_bracket) str
  and nt_paired_comment str = (pack (caten (caten nt_left_curly_bracket
                                        (star (disj_list [(pack nt_symbol (fun _ -> ()));
                                         (pack nt_paired_comment (fun _ -> ()));
                                         (pack nt_legit_paired_comment_chars (fun _ -> ()))]))
                                    ) nt_right_curly_bracket) (fun _ -> ())) str
  and nt_comment str =
    disj_list
      [nt_line_comment;
       nt_paired_comment;
       nt_sexpr_comment] str
  and nt_void str =
    let nt1 = word_ci "#void" in
    let nt1 = not_followed_by nt1 nt_symbol_char in
    let nt1 = pack nt1 (fun _ -> ScmVoid) in
    nt1 str
  and nt_skip_star str =
    let nt1 = disj (unitify nt_whitespace) nt_comment in
    let nt1 = unitify (star nt1) in
    nt1 str
  and make_skipped_star (nt : 'a parser) =
    let nt1 = caten nt_skip_star (caten nt nt_skip_star) in
    let nt1 = pack nt1 (fun (_, (e, _)) -> e) in
    nt1
  and is_decimal_digit (d: char) = d >= '0' && d <= '9'
  and is_hex_digit (d: char) = is_decimal_digit d || ('A' <= d && d <= 'F')  || ('a' <= d && d <= 'f')
  and decimal_digit_numeric_value (d: char) = (int_of_char d - int_of_char '0')
  and hex_digit_numeric_value  (d: char) = if (is_decimal_digit d) then (decimal_digit_numeric_value d)
                                          else match d with
                                              | 'A' | 'a' -> 10
                                              | 'B' | 'b' -> 11
                                              | 'C' | 'c' -> 12
                                              | 'D' | 'd' -> 13
                                              | 'E' | 'e' -> 14
                                              | 'F' | 'f' -> 15
                                              | _ -> raise X_no_match
  and nt_digit str = pack (range '0' '9') decimal_digit_numeric_value str
  and nt_hex_digit str = pack (const is_hex_digit) hex_digit_numeric_value str
  and nt_nat str =
    let nt1 = plus nt_digit in
    let nt1 = pack nt1
                (fun digits ->
                  List.fold_left
                    (fun num digit ->
                      10 * num + digit)
                    0
                    digits) in
    nt1 str
  and nt_hex_nat str =
    let nt1 = plus nt_hex_digit in
    let nt1 = pack nt1
                (fun digits ->
                  List.fold_left
                    (fun num digit ->
                      16 * num + digit)
                    0
                    digits) in
    nt1 str
  and nt_optional_sign str =
   (pack (make_maybe (disj (char '+') (char '-')) '+')
    (fun c -> match c with
        | '+' -> true
        | '-' -> false
        | _ -> raise X_no_match))
    str
  and nt_int str =
    let nt1 = caten nt_optional_sign nt_nat in
    let nt1 = pack nt1
                (fun (is_positive, n) ->
                  if is_positive then n else -n) in
    nt1 str
  and nt_frac str =
    let nt1 = caten nt_int (char '/') in
    let nt1 = pack nt1 (fun (num, _) -> num) in
    let nt2 = only_if nt_nat (fun n -> n != 0) in
    let nt1 = caten nt1 nt2 in
    let nt1 = pack nt1
                (fun (num, den) ->
                  let d = gcd num den in
                  ScmRational(num / d, den / d)) in
    nt1 str
  and nt_integer_part str =
    let nt1 = plus nt_digit in
    let nt1 = pack nt1
                (fun digits ->
                  List.fold_left
                    (fun num digit -> 10.0 *. num +. (float_of_int digit))
                    0.0
                    digits) in
    nt1 str
  and nt_mantissa str =
    let nt1 = plus nt_digit in
    let nt1 = pack nt1
                (fun digits ->
                  List.fold_right
                    (fun digit num ->
                      ((float_of_int digit) +. num) /. 10.0)
                    digits
                    0.0) in
    nt1 str
  and nt_exponent str =
    let nt1 = unitify (char_ci 'e') in
    let nt2 = word "*10" in
    let nt3 = unitify (word "**") in
    let nt4 = unitify (char '^') in
    let nt3 = disj nt3 nt4 in
    let nt2 = caten nt2 nt3 in
    let nt2 = unitify nt2 in
    let nt1 = disj nt1 nt2 in
    let nt1 = caten nt1 nt_int in
    let nt1 = pack nt1 (fun (_, n) -> Float.pow 10. (float_of_int n)) in
    nt1 str
  and nt_dot =
      char '.'
   and nt_optional_mantissa str =
      (make_maybe nt_mantissa 0.) str
  and nt_optional_exp str =
    (make_maybe nt_exponent 1.) str
 and nt_float_a str = (pack (caten (pack (caten (pack
                                                (caten nt_integer_part nt_dot)
                                                 (fun (integer_part, dot) -> integer_part)
                                                )
                                        nt_optional_mantissa
                                        )
                                   (fun (integer_part, mantissa) -> integer_part +. mantissa)
                                 ) nt_optional_exp
                            )
                        (fun (non_exp_part, exp_part) -> non_exp_part *. exp_part)
                      ) str
 and nt_float_b str = (pack (caten (pack (caten nt_dot nt_mantissa) (fun (dot, mantissa) -> mantissa)
                                  ) nt_optional_exp
                           ) (fun (mantissa, exp) -> mantissa *. exp)
                      ) str
  and nt_float_c str = (pack (caten nt_integer_part nt_exponent)
                        (fun (integer_part, exp) -> integer_part *. exp)
                       ) str
  and nt_unsigned_float str = (disj_list [nt_float_a; nt_float_b; nt_float_c;]) str
  and nt_float_internal str = (pack (caten nt_optional_sign nt_unsigned_float)
                         (fun (is_positive, num) -> if is_positive then num else -.num)
                     ) str
  and nt_float str = (pack nt_float_internal (fun (num) -> ScmReal num)) str
  and nt_number str =
    let nt1 = nt_float in
    let nt2 = nt_frac in
    let nt3 = pack nt_int (fun n -> ScmRational(n, 1)) in
    let nt1 = disj nt1 (disj nt2 nt3) in
    let nt1 = pack nt1 (fun r -> ScmNumber r) in
    let nt1 = not_followed_by nt1 nt_symbol_char in
    nt1 str
  and nt_boolean str =
    let nt1 = char '#' in
    let nt2 = char_ci 'f' in
    let nt2 = pack nt2 (fun _ -> ScmBoolean false) in
    let nt3 = char_ci 't' in
    let nt3 = pack nt3 (fun _ -> ScmBoolean true) in
    let nt2 = disj nt2 nt3 in
    let nt1 = caten nt1 nt2 in
    let nt1 = pack nt1 (fun (_, value) -> value) in
    let nt2 = nt_symbol_char in
    let nt1 = not_followed_by nt1 nt2 in
    nt1 str
  and nt_char_simple str =
    let nt1 = const(fun ch -> ' ' < ch) in
    let nt1 = not_followed_by nt1 nt_symbol_char in
    nt1 str

  and nt_char_named str = (disj_list [ascii_char 10; ascii_char 0; ascii_char 12;
                              ascii_char 13; ascii_char 32; ascii_char 9;]) str
  and nt_char_hex str =
    let nt1 = caten (char_ci 'x') nt_hex_nat in
    let nt1 = pack nt1 (fun (_, n) -> n) in
    let nt1 = only_if nt1 (fun n -> n < 256) in
    let nt1 = pack nt1 (fun n -> char_of_int n) in
    nt1 str
  and nt_char str =
    let nt1 = word "#\\" in
    let nt2 = disj nt_char_simple (disj nt_char_named nt_char_hex) in
    let nt1 = caten nt1 nt2 in
    let nt1 = pack nt1 (fun (_, ch) -> ScmChar ch) in
    nt1 str
  and nt_symbol_char str =
    let nt1 = range_ci 'a' 'z' in
    let nt1 = pack nt1 Char.lowercase_ascii in
    let nt2 = range '0' '9' in
    let nt3 = one_of "!$^*_-+=<>?/" in
    let nt1 = disj nt1 (disj nt2 nt3) in
    nt1 str

  and nt_symbol str = (pack (plus nt_symbol_char) (fun (chars) -> ScmSymbol (string_of_list chars))) str
  and nt_string_part_simple str =
    let nt1 =
      disj_list [unitify (char '"'); unitify (char '\\'); unitify (word "~~");
                 unitify nt_string_part_dynamic] in
    let nt1 = diff nt_any nt1 in
    nt1 str
  and nt_string_part_meta str =
    let nt1 =
      disj_list [pack (word "\\\\") (fun _ -> '\\');
                 pack (word "\\\"") (fun _ -> '"');
                 pack (word "\\n") (fun _ -> '\n');
                 pack (word "\\r") (fun _ -> '\r');
                 pack (word "\\f") (fun _ -> '\012');
                 pack (word "\\t") (fun _ -> '\t');
                 pack (word "~~") (fun _ -> '~')] in
    nt1 str
  and nt_string_part_hex str =
    let nt1 = word_ci "\\x" in
    let nt2 = nt_hex_nat in
    let nt2 = only_if nt2 (fun n -> n < 256) in
    let nt3 = char ';' in
    let nt1 = caten nt1 (caten nt2 nt3) in
    let nt1 = pack nt1 (fun (_, (n, _)) -> n) in
    let nt1 = pack nt1 char_of_int in
    nt1 str
  and fix_sexp_in_dynamic_string_part exp = Dynamic (ScmPair(
                                                        ScmSymbol("format"),
                                                        ScmPair(
                                                            ScmString("~a"),
                                                            exp
                                                        )
                                                     ))

  and nt_tilda str = (non_escaped '~') str
  and nt_string_part_dynamic str =
    let nt1 = pack (caten nt_tilda nt_left_curly_bracket) (fun (s1, s2) -> s1) in
    let nt1 = pack (caten nt1 nt_sexpr) (fun (s, exp) -> fix_sexp_in_dynamic_string_part exp) in
    let nt1 = pack (caten nt1 nt_right_curly_bracket) (fun (fixed_exp, s) -> fixed_exp) in
    nt1 str

  and nt_string_part_static str =
    let nt1 = disj_list [nt_string_part_simple;
                         nt_string_part_meta;
                         nt_string_part_hex] in
    let nt1 = plus nt1 in
    let nt1 = pack nt1 string_of_list in
    let nt1 = pack nt1 (fun str -> Static str) in
    nt1 str
  and nt_string_part str =
    disj nt_string_part_static nt_string_part_dynamic str
  and nt_string str =
    let nt1 = char '"' in
    let nt2 = star nt_string_part in
    let nt3 = char '"' in
    let nt1 = caten nt1 (caten nt2 nt3) in
    let nt1 = pack nt1 (fun (_, (parts, _)) -> parts) in
    let nt1 = pack nt1
                (fun parts ->
                  match parts with
                  | [] -> ScmString ""
                  | [Static(str)] -> ScmString str
                  | [Dynamic(sexpr)] -> sexpr
                  | parts ->
                     let argl =
                       List.fold_right
                         (fun car cdr ->
                           ScmPair((match car with
                                    | Static(str) -> ScmString(str)
                                    | Dynamic(sexpr) -> sexpr),
                                   cdr))
                         parts
                         ScmNil in
                     ScmPair(ScmSymbol "string-append", argl)) in
    nt1 str

  and nt_hash str = (non_escaped '#') str
  and nt_left_round_bracket str = (non_escaped '(') str
  and nt_right_round_bracket str = (non_escaped ')') str
  and nt_vector str =
    let nt1 = pack (caten nt_hash nt_left_round_bracket) (fun (s1, s2) -> s1) in
    let nt1 = pack (caten nt1 (star nt_sexpr)) (fun (s, sexpressions) -> sexpressions) in
    let nt1 = pack (caten nt1 nt_right_round_bracket) (fun (sexpressions, s) -> ScmVector(sexpressions)) in
    nt1 str
  and nt_list str =
    let nt1 = char '(' in
    let nt2 = pack (caten nt_skip_star (char ')')) (fun _ -> ScmNil) in
    let nt3 = plus nt_sexpr in
    let nt4 = pack (char ')') (fun _ -> ScmNil) in
    let nt5 = pack (caten (char '.') (caten nt_sexpr (char ')')))
                   (fun (_, (sexpr, _)) -> sexpr) in
    let nt4 = disj nt4 nt5 in
    let nt3 = pack (caten nt3 nt4)
        (fun (sexprs, sexpr) ->
            List.fold_right
                   (fun car cdr -> ScmPair (car, cdr))
                   sexprs
                   sexpr) in
    let nt2 = disj nt2 nt3 in
    let nt1 = pack (caten nt1 nt2) (fun (_, sexpr) -> sexpr) in
    (make_skipped_star nt1) str
  and make_quoted_form nt_qf qf_name =
    let nt1 = caten nt_qf nt_sexpr in
    let nt1 = pack nt1
                (fun (_, sexpr) ->
                  ScmPair(ScmSymbol qf_name,
                          ScmPair(sexpr, ScmNil))) in
    nt1
  and nt_quoted_forms str =
    let nt1 =
      disj_list [(make_quoted_form (unitify (char '\'')) "quote");
                 (make_quoted_form (unitify (char '`')) "quasiquote");
                 (make_quoted_form
                    (unitify (not_followed_by (char ',') (char '@')))
                    "unquote");
                 (make_quoted_form (unitify (word ",@")) "unquote-splicing")] in
    nt1 str
  and nt_sexpr str =
    let nt1 =
      disj_list [nt_void; nt_number; nt_boolean; nt_char; nt_symbol;
                 nt_string; nt_vector; nt_list; nt_quoted_forms] in
    let nt1 = make_skipped_star nt1 in
    nt1 str;;

  let rec string_of_sexpr = function
    | ScmVoid -> "#<void>"
    | ScmNil -> "()"
    | ScmBoolean(false) -> "#f"
    | ScmBoolean(true) -> "#t"
    | ScmChar('\n') -> "#\\newline"
    | ScmChar('\r') -> "#\\return"
    | ScmChar('\012') -> "#\\page"
    | ScmChar('\t') -> "#\\tab"
    | ScmChar(' ') -> "#\\space"
    | ScmChar(ch) ->
       if (ch < ' ')
       then let n = int_of_char ch in
            Printf.sprintf "#\\x%x" n
       else Printf.sprintf "#\\%c" ch
    | ScmString(str) ->
       Printf.sprintf "\"%s\""
         (String.concat ""
            (List.map
               (function
                | '\n' -> "\\n"
                | '\012' -> "\\f"
                | '\r' -> "\\r"
                | '\t' -> "\\t"
                | '\"' -> "\\\""
                | ch ->
                   if (ch < ' ')
                   then Printf.sprintf "\\x%x;" (int_of_char ch)
                   else Printf.sprintf "%c" ch)
               (list_of_string str)))
    | ScmSymbol(sym) -> sym
    | ScmNumber(ScmRational(0, _)) -> "0"
    | ScmNumber(ScmRational(num, 1)) -> Printf.sprintf "%d" num
    | ScmNumber(ScmRational(num, -1)) -> Printf.sprintf "%d" (- num)
    | ScmNumber(ScmRational(num, den)) -> Printf.sprintf "%d/%d" num den
    | ScmNumber(ScmReal(x)) -> Printf.sprintf "%f" x
    | ScmVector(sexprs) ->
       let strings = List.map string_of_sexpr sexprs in
       let inner_string = String.concat " " strings in
       Printf.sprintf "#(%s)" inner_string
    | ScmPair(ScmSymbol "quote",
              ScmPair(sexpr, ScmNil)) ->
       Printf.sprintf "'%s" (string_of_sexpr sexpr)
    | ScmPair(ScmSymbol "quasiquote",
              ScmPair(sexpr, ScmNil)) ->
       Printf.sprintf "`%s" (string_of_sexpr sexpr)
    | ScmPair(ScmSymbol "unquote",
              ScmPair(sexpr, ScmNil)) ->
       Printf.sprintf ",%s" (string_of_sexpr sexpr)
    | ScmPair(ScmSymbol "unquote-splicing",
              ScmPair(sexpr, ScmNil)) ->
       Printf.sprintf ",@%s" (string_of_sexpr sexpr)
    | ScmPair(car, cdr) ->
       string_of_sexpr' (string_of_sexpr car) cdr
  and string_of_sexpr' car_string = function
    | ScmNil -> Printf.sprintf "(%s)" car_string
    | ScmPair(cadr, cddr) ->
       let new_car_string =
         Printf.sprintf "%s %s" car_string (string_of_sexpr cadr) in
       string_of_sexpr' new_car_string cddr
    | cdr ->
       let cdr_string = (string_of_sexpr cdr) in
       Printf.sprintf "(%s . %s)" car_string cdr_string;;

  let print_sexpr chan sexpr = output_string chan (string_of_sexpr sexpr);;

  let print_sexprs chan sexprs =
    output_string chan
      (Printf.sprintf "[%s]"
         (String.concat "; "
            (List.map string_of_sexpr sexprs)));;

  let sprint_sexpr _ sexpr = string_of_sexpr sexpr;;

  let sprint_sexprs chan sexprs =
    Printf.sprintf "[%s]"
      (String.concat "; "
         (List.map string_of_sexpr sexprs));;

  let scheme_sexpr_list_of_sexpr_list sexprs =
    List.fold_right (fun car cdr -> ScmPair (car, cdr)) sexprs ScmNil;;

end;; (* end of struct Reader *)

(* the tag-parser *)

exception X_syntax of string;;

type var = 
  | Var of string;;

type lambda_kind =
  | Simple
  | Opt of string;;

type expr =
  | ScmConst of sexpr
  | ScmVarGet of var
  | ScmIf of expr * expr * expr
  | ScmSeq of expr list
  | ScmOr of expr list
  | ScmVarSet of var * expr
  | ScmVarDef of var * expr
  | ScmLambda of string list * lambda_kind * expr
  | ScmApplic of expr * expr list;;

module type TAG_PARSER = sig
  val tag_parse : sexpr -> expr
  val print_expr : out_channel -> expr -> unit
  val print_exprs : out_channel -> expr list -> unit
  val sprint_expr : 'a -> expr -> string
  val sprint_exprs : 'a -> expr list -> string    
end;;

module Tag_Parser = struct
  open Reader;;
  
  let reserved_word_list =
    ["and"; "begin"; "cond"; "do"; "else"; "if"; "lambda";
     "let"; "let*"; "letrec"; "or"; "quasiquote"; "quote";
     "unquote"; "unquote-splicing"];;

  let rec scheme_list_to_ocaml = function
    | ScmNil -> ([], ScmNil)
    | ScmPair(car, cdr) ->
       ((fun (rdc, last) -> (car :: rdc, last))
          (scheme_list_to_ocaml cdr))  
    | rac -> ([], rac);;

  let is_reserved_word name = is_member name reserved_word_list;;

  let unsymbolify_var = function
    | ScmSymbol var -> var
    | _ -> raise (X_syntax "not a symbol");;

  let unsymbolify_vars = List.map unsymbolify_var;;

  let list_contains_unquote_splicing =
    ormap (function
        | ScmPair (ScmSymbol "unquote-splicing",
                   ScmPair (_, ScmNil)) -> true
        | _ -> false);;

  let rec macro_expand_qq = function
    | ScmNil -> ScmPair (ScmSymbol "quote", ScmPair (ScmNil, ScmNil))
    | (ScmSymbol _) as sexpr ->
       ScmPair (ScmSymbol "quote", ScmPair (sexpr, ScmNil))
    | ScmPair (ScmSymbol "unquote", ScmPair (sexpr, ScmNil)) -> sexpr
    | ScmPair (ScmPair (ScmSymbol "unquote",
                        ScmPair (car, ScmNil)),
               cdr) ->
       let cdr = macro_expand_qq cdr in
       ScmPair (ScmSymbol "cons", ScmPair (car, ScmPair (cdr, ScmNil)))
    | ScmPair (ScmPair (ScmSymbol "unquote-splicing",
                        ScmPair (sexpr, ScmNil)),
               ScmNil) ->
       sexpr
    | ScmPair (ScmPair (ScmSymbol "unquote-splicing",
                        ScmPair (car, ScmNil)), cdr) ->
       let cdr = macro_expand_qq cdr in
       ScmPair (ScmSymbol "append",
                ScmPair (car, ScmPair (cdr, ScmNil)))
    | ScmPair (car, cdr) ->
       let car = macro_expand_qq car in
       let cdr = macro_expand_qq cdr in
       ScmPair
         (ScmSymbol "cons",
          ScmPair (car, ScmPair (cdr, ScmNil)))
    | ScmVector sexprs ->
       if (list_contains_unquote_splicing sexprs)
       then let sexpr = macro_expand_qq
                          (scheme_sexpr_list_of_sexpr_list sexprs) in
            ScmPair (ScmSymbol "list->vector",
                     ScmPair (sexpr, ScmNil))
       else let sexprs = 
              (scheme_sexpr_list_of_sexpr_list
                 (List.map macro_expand_qq sexprs)) in
            ScmPair (ScmSymbol "vector", sexprs)
    | sexpr -> sexpr;;

 let rec macro_expand_and_clauses expr = function
    | [] -> expr
    | expr' :: exprs ->
       ScmPair (ScmSymbol "if",
                ScmPair (expr, 
                         ScmPair ( (macro_expand_and_clauses expr' exprs), 
                                  ScmPair (ScmBoolean(false), ScmNil))));;
 

  let rec macro_expand_cond_ribs ribs =
    match ribs with
    | ScmNil -> ScmNil
    | ScmPair (ScmPair (ScmSymbol "else", exprs), ribs) -> ScmPair((ScmSymbol "begin"), exprs)
    | ScmPair (ScmPair (expr,
                        ScmPair (ScmSymbol "=>",
                                 ScmPair (func, ScmNil))),
               ribs) ->
       let remaining = macro_expand_cond_ribs ribs in
       ScmPair
         (ScmSymbol "let",
          ScmPair
            (ScmPair
               (ScmPair (ScmSymbol "value", ScmPair (expr, ScmNil)),
                ScmPair
                  (ScmPair
                     (ScmSymbol "f",
                      ScmPair
                        (ScmPair
                           (ScmSymbol "lambda",
                            ScmPair (ScmNil, ScmPair (func, ScmNil))),
                         ScmNil)),
                   ScmPair
                     (ScmPair
                        (ScmSymbol "rest",
                         ScmPair
                           (ScmPair
                              (ScmSymbol "lambda",
                               ScmPair (ScmNil, ScmPair (remaining, ScmNil))),
                            ScmNil)),
                      ScmNil))),
             ScmPair
               (ScmPair
                  (ScmSymbol "if",
                   ScmPair
                     (ScmSymbol "value",
                      ScmPair
                        (ScmPair
                           (ScmPair (ScmSymbol "f", ScmNil),
                            ScmPair (ScmSymbol "value", ScmNil)),
                         ScmPair (ScmPair (ScmSymbol "rest", ScmNil), ScmNil)))),
                ScmNil)))
    | ScmPair (ScmPair (pred, exprs), ribs) ->
       let remaining = macro_expand_cond_ribs ribs in
       ScmPair (ScmSymbol "if",
                ScmPair (pred,
                         ScmPair
                           (ScmPair (ScmSymbol "begin", exprs),
                            ScmPair (remaining, ScmNil))))
    | _ -> raise (X_syntax "malformed cond-rib");;


  let rec let_ribs_to_vars ribs =
    match ribs with
    | [] -> []
    | (var_name , value) :: rest -> [var_name] @ (let_ribs_to_vars rest);;


  let rec let_ribs_to_vals ribs =
    match ribs with
    | [] -> []
    | (var_name , value) :: rest -> [value] @ (let_ribs_to_vars rest);;

  let rec tag_parse sexpr =
    match sexpr with
    | ScmVoid | ScmBoolean _ | ScmChar _ | ScmString _ | ScmNumber _ ->
       ScmConst sexpr
    | ScmPair (ScmSymbol "quote", ScmPair (sexpr, ScmNil)) ->
       ScmConst sexpr
    | ScmPair (ScmSymbol "quasiquote", ScmPair (sexpr, ScmNil)) ->
       tag_parse (macro_expand_qq sexpr)
    | ScmSymbol var ->
       if (is_reserved_word var)
       then raise (X_syntax "Variable cannot be a reserved word")
       else ScmVarGet(Var var)
    | ScmPair (ScmSymbol "if",
               ScmPair (test, ScmPair (dit, ScmNil))) ->
       ScmIf(tag_parse test,
             tag_parse dit,
             tag_parse ScmVoid)
    | ScmPair (ScmSymbol "if",
               ScmPair (test, ScmPair (dit, ScmPair (dif, ScmNil)))) ->
       ScmIf(tag_parse test,
             tag_parse dit,
             tag_parse dif)
    | ScmPair (ScmSymbol "begin", ScmNil) -> ScmConst(ScmVoid)
    | ScmPair (ScmSymbol "begin", ScmPair (sexpr, ScmNil)) ->
       tag_parse sexpr
    | ScmPair (ScmSymbol "begin", sexprs) ->
       (match (scheme_list_to_ocaml sexprs) with
        | (sexprs', ScmNil) -> ScmSeq(List.map tag_parse sexprs')
        | _ -> raise (X_syntax "Improper sequence"))
    | ScmPair (ScmSymbol "set!",
               ScmPair (ScmSymbol var,
                        ScmPair (expr, ScmNil))) ->
       if (is_reserved_word var)
       then raise (X_syntax "cannot assign a reserved word")
       else ScmVarSet(Var var, tag_parse expr)
    | ScmPair (ScmSymbol "define", ScmPair (ScmPair (var, params), exprs)) ->
       tag_parse
         (ScmPair (ScmSymbol "define",
                   ScmPair (var,
                            ScmPair (ScmPair (ScmSymbol "lambda",
                                              ScmPair (params, exprs)),
                                     ScmNil))))
    | ScmPair (ScmSymbol "define",
               ScmPair (ScmSymbol var,
                        ScmPair (expr, ScmNil))) ->
       if (is_reserved_word var)
       then raise (X_syntax "cannot define a reserved word")
       else ScmVarDef(Var var, tag_parse expr)
    | ScmPair (ScmSymbol "lambda", ScmPair (params, exprs)) ->
       let expr = tag_parse (ScmPair(ScmSymbol "begin", exprs)) in
       (match (scheme_list_to_ocaml params) with
        | params, ScmNil -> ScmLambda(unsymbolify_vars params, Simple, expr)
        | params, ScmSymbol opt ->
           ScmLambda(unsymbolify_vars params, Opt opt, expr)
        | _ -> raise (X_syntax "invalid parameter list"))
    | ScmPair (ScmSymbol "let", ScmPair (ribs, exprs)) -> raise X_not_yet_implemented
    | ScmPair (ScmSymbol "let*", ScmPair (ScmNil, exprs)) -> raise X_not_yet_implemented (*ScmApplic (ScmLambda ([], Simple, exprs), [])*)
    | ScmPair (ScmSymbol "let*",
               ScmPair
                 (ScmPair
                    (ScmPair (var, ScmPair (value, ScmNil)), ScmNil),
                  exprs)) -> raise X_not_yet_implemented
    | ScmPair (ScmSymbol "let*",
               ScmPair (ScmPair (ScmPair (var,
                                          ScmPair (arg, ScmNil)),
                                 ribs),
                        exprs)) -> raise X_not_yet_implemented
    | ScmPair (ScmSymbol "letrec", ScmPair (ribs, exprs)) ->
       raise X_not_yet_implemented
    | ScmPair (ScmSymbol "and", ScmNil) -> ScmConst (ScmBoolean true)
    | ScmPair (ScmSymbol "and", exprs) ->
      (match (scheme_list_to_ocaml exprs) with
      | expr :: exprs, ScmNil ->
         tag_parse (macro_expand_and_clauses expr exprs))
    | ScmPair (ScmSymbol "cond", ribs) ->
       tag_parse (macro_expand_cond_ribs ribs)
    | ScmPair (proc, args) ->
       let proc =
         (match proc with
          | ScmSymbol var ->
             if (is_reserved_word var)
             then raise (X_syntax "reserved word in proc position")
             else proc
          | proc -> proc) in
       (match (scheme_list_to_ocaml args) with
        | args, ScmNil ->
           ScmApplic (tag_parse proc, List.map tag_parse args)
        | _ -> raise (X_syntax "malformed application"))
    | sexpr -> raise (X_syntax
                       (Printf.sprintf
                          "Unknown form: \n%a\n"
                          Reader.sprint_sexpr sexpr));;

  let rec sexpr_of_expr = function
    | ScmConst(ScmVoid) -> ScmVoid
    | ScmConst((ScmBoolean _) as sexpr) -> sexpr
    | ScmConst((ScmChar _) as sexpr) -> sexpr
    | ScmConst((ScmString _) as sexpr) -> sexpr
    | ScmConst((ScmNumber _) as sexpr) -> sexpr
    | ScmConst((ScmSymbol _) as sexpr) ->
       ScmPair (ScmSymbol "quote", ScmPair (sexpr, ScmNil))
    | ScmConst(ScmNil as sexpr) ->
       ScmPair (ScmSymbol "quote", ScmPair (sexpr, ScmNil))
    | ScmConst((ScmVector _) as sexpr) ->
       ScmPair (ScmSymbol "quote", ScmPair (sexpr, ScmNil))  
    | ScmVarGet(Var var) -> ScmSymbol var
    | ScmIf(test, dit, ScmConst ScmVoid) ->
       let test = sexpr_of_expr test in
       let dit = sexpr_of_expr dit in
       ScmPair (ScmSymbol "if", ScmPair (test, ScmPair (dit, ScmNil)))
    | ScmIf(e1, e2, ScmConst (ScmBoolean false)) ->
       let e1 = sexpr_of_expr e1 in
       (match (sexpr_of_expr e2) with
        | ScmPair (ScmSymbol "and", exprs) ->
           ScmPair (ScmSymbol "and", ScmPair(e1, exprs))
        | e2 -> ScmPair (ScmSymbol "and", ScmPair (e1, ScmPair (e2, ScmNil))))
    | ScmIf(test, dit, dif) ->
       let test = sexpr_of_expr test in
       let dit = sexpr_of_expr dit in
       let dif = sexpr_of_expr dif in
       ScmPair
         (ScmSymbol "if", ScmPair (test, ScmPair (dit, ScmPair (dif, ScmNil))))
    | ScmSeq([]) -> ScmVoid
    | ScmSeq([expr]) -> sexpr_of_expr expr
    | ScmSeq(exprs) ->
       ScmPair(ScmSymbol "begin", 
               scheme_sexpr_list_of_sexpr_list
                 (List.map sexpr_of_expr exprs))
    | ScmVarSet(Var var, expr) ->
       let var = ScmSymbol var in
       let expr = sexpr_of_expr expr in
       ScmPair (ScmSymbol "set!", ScmPair (var, ScmPair (expr, ScmNil)))
    | ScmVarDef(Var var, expr) ->
       let var = ScmSymbol var in
       let expr = sexpr_of_expr expr in
       ScmPair (ScmSymbol "define", ScmPair (var, ScmPair (expr, ScmNil)))
    | ScmLambda(params, Simple, expr) ->
       let params = scheme_sexpr_list_of_sexpr_list
                      (List.map (fun str -> ScmSymbol str) params) in
       let expr = sexpr_of_expr expr in
       ScmPair (ScmSymbol "lambda",
                ScmPair (params,
                         ScmPair (expr, ScmNil)))
    | ScmLambda([], Opt opt, expr) ->
       let expr = sexpr_of_expr expr in
       let opt = ScmSymbol opt in
       ScmPair
         (ScmSymbol "lambda",
          ScmPair (opt, ScmPair (expr, ScmNil)))
    | ScmLambda(params, Opt opt, expr) ->
       let expr = sexpr_of_expr expr in
       let opt = ScmSymbol opt in
       let params = List.fold_right
                      (fun param sexpr -> ScmPair(ScmSymbol param, sexpr))
                      params
                      opt in
       ScmPair
         (ScmSymbol "lambda", ScmPair (params, ScmPair (expr, ScmNil)))
    | ScmApplic (ScmLambda (params, Simple, expr), args) ->
       let ribs =
         scheme_sexpr_list_of_sexpr_list
           (List.map2
              (fun param arg -> ScmPair (ScmSymbol param, ScmPair (arg, ScmNil)))
              params
              (List.map sexpr_of_expr args)) in
       let expr = sexpr_of_expr expr in
       ScmPair
         (ScmSymbol "let",
          ScmPair (ribs,
                   ScmPair (expr, ScmNil)))
    | ScmApplic (proc, args) ->
       let proc = sexpr_of_expr proc in
       let args =
         scheme_sexpr_list_of_sexpr_list
           (List.map sexpr_of_expr args) in
       ScmPair (proc, args)
    | _ -> raise (X_syntax "Unknown form");;

  let string_of_expr expr =
    Printf.sprintf "%a" sprint_sexpr (sexpr_of_expr expr);;

  let print_expr chan expr =
    output_string chan
      (string_of_expr expr);;

  let print_exprs chan exprs =
    output_string chan
      (Printf.sprintf "[%s]"
         (String.concat "; "
            (List.map string_of_expr exprs)));;

  let sprint_expr _ expr = string_of_expr expr;;

  let sprint_exprs chan exprs =
    Printf.sprintf "[%s]"
      (String.concat "; "
         (List.map string_of_expr exprs));;

end;; (* end of struct Tag_Parser *)

type app_kind = Tail_Call | Non_Tail_Call;;

type lexical_address =
  | Free
  | Param of int
  | Bound of int * int;;

type var' = Var' of string * lexical_address;;

type expr' =
  | ScmConst' of sexpr
  | ScmVarGet' of var'
  | ScmIf' of expr' * expr' * expr'
  | ScmSeq' of expr' list
  | ScmOr' of expr' list
  | ScmVarSet' of var' * expr'
  | ScmVarDef' of var' * expr'
  | ScmBox' of var'
  | ScmBoxGet' of var'
  | ScmBoxSet' of var' * expr'
  | ScmLambda' of string list * lambda_kind * expr'
  | ScmApplic' of expr' * expr' list * app_kind;;

module type SEMANTIC_ANALYSIS = sig
  val annotate_lexical_address : expr -> expr'
  val annotate_tail_calls : expr' -> expr'
  val auto_box : expr' -> expr'
  val semantics : expr -> expr'  
end;; (* end of signature SEMANTIC_ANALYSIS *)

module Semantic_Analysis : SEMANTIC_ANALYSIS = struct

  let rec lookup_in_rib name = function
    | [] -> None
    | name' :: rib ->
       if name = name'
       then Some(0)
       else (match (lookup_in_rib name rib) with
             | None -> None
             | Some minor -> Some (minor + 1));;

  let rec lookup_in_env name = function
    | [] -> None
    | rib :: env ->
       (match (lookup_in_rib name rib) with
        | None ->
           (match (lookup_in_env name env) with
            | None -> None
            | Some(major, minor) -> Some(major + 1, minor))
        | Some minor -> Some(0, minor));;

  let tag_lexical_address_for_var name params env = 
    match (lookup_in_rib name params) with
    | None ->
       (match (lookup_in_env name env) with
        | None -> Var' (name, Free)
        | Some(major, minor) -> Var' (name, Bound (major, minor)))
    | Some minor -> Var' (name, Param minor);;

  (* run this first *)
  let annotate_lexical_address =
    let rec run expr params env =
      match expr with
      | ScmConst sexpr -> raise X_not_yet_implemented
      | ScmVarGet (Var str) -> raise X_not_yet_implemented
      | ScmIf (test, dit, dif) -> raise X_not_yet_implemented
      | ScmSeq exprs -> raise X_not_yet_implemented
      | ScmOr exprs -> raise X_not_yet_implemented
      | ScmVarSet(Var v, expr) -> raise X_not_yet_implemented
      (* this code does not [yet?] support nested define-expressions *)
      | ScmVarDef(Var v, expr) -> raise X_not_yet_implemented
      | ScmLambda (params', Simple, expr) -> raise X_not_yet_implemented
      | ScmLambda (params', Opt opt, expr) -> raise X_not_yet_implemented
      | ScmApplic (proc, args) ->
         ScmApplic' (run proc params env,
                     List.map (fun arg -> run arg params env) args,
                     Non_Tail_Call)
    in
    fun expr ->
    run expr [] [];;

  (* run this second *)
  let annotate_tail_calls = 
    let rec run in_tail = function
      | (ScmConst' _) as orig -> raise X_not_yet_implemented
      | (ScmVarGet' _) as orig -> raise X_not_yet_implemented
      | ScmIf' (test, dit, dif) -> raise X_not_yet_implemented
      | ScmSeq' [] -> raise X_not_yet_implemented
      | ScmSeq' (expr :: exprs) -> raise X_not_yet_implemented
      | ScmOr' [] -> raise X_not_yet_implemented
      | ScmOr' (expr :: exprs) -> raise X_not_yet_implemented
      | ScmVarSet' (var', expr') -> raise X_not_yet_implemented
      | ScmVarDef' (var', expr') -> raise X_not_yet_implemented
      | (ScmBox' _) as expr' -> raise X_not_yet_implemented
      | (ScmBoxGet' _) as expr' -> raise X_not_yet_implemented
      | ScmBoxSet' (var', expr') -> raise X_not_yet_implemented
      | ScmLambda' (params, Simple, expr) -> raise X_not_yet_implemented
      | ScmLambda' (params, Opt opt, expr) -> raise X_not_yet_implemented
      | ScmApplic' (proc, args, app_kind) ->
         if in_tail
         then ScmApplic' (run false proc,
                          List.map (fun arg -> run false arg) args,
                          Tail_Call)
         else ScmApplic' (run false proc,
                          List.map (fun arg -> run false arg) args,
                          Non_Tail_Call)
    and runl in_tail expr = function
      | [] -> [run in_tail expr]
      | expr' :: exprs -> (run false expr) :: (runl in_tail expr' exprs)
    in
    fun expr' -> raise X_not_yet_implemented;;

  (* auto_box *)

  let copy_list = List.map (fun si -> si);;

  let combine_pairs =
    List.fold_left
      (fun (rs1, ws1) (rs2, ws2) -> (rs1 @ rs2, ws1 @ ws2))
      ([], []);;

  let find_reads_and_writes =
    let rec run name expr params env =
      match expr with
      | ScmConst' _ -> ([], [])
      | ScmVarGet' (Var' (_, Free)) -> ([], [])
      | ScmVarGet' (Var' (name', _) as v) ->
         if name = name'
         then ([(v, env)], [])
         else ([], [])
      | ScmBox' _ -> ([], [])
      | ScmBoxGet' _ -> ([], [])
      | ScmBoxSet' (_, expr) -> run name expr params env
      | ScmIf' (test, dit, dif) ->
         let (rs1, ws1) = (run name test params env) in
         let (rs2, ws2) = (run name dit params env) in
         let (rs3, ws3) = (run name dif params env) in
         (rs1 @ rs2 @ rs3, ws1 @ ws2 @ ws3)
      | ScmSeq' exprs ->
         combine_pairs
           (List.map
              (fun expr -> run name expr params env)
              exprs)
      | ScmVarSet' (Var' (_, Free), expr) -> run name expr params env
      | ScmVarSet' ((Var' (name', _) as v), expr) ->
         let (rs1, ws1) =
           if name = name'
           then ([], [(v, env)])
           else ([], []) in
         let (rs2, ws2) = run name expr params env in
         (rs1 @ rs2, ws1 @ ws2)
      | ScmVarDef' (_, expr) -> run name expr params env
      | ScmOr' exprs ->
         combine_pairs
           (List.map
              (fun expr -> run name expr params env)
              exprs)
      | ScmLambda' (params', Simple, expr) ->
         if (List.mem name params')
         then ([], [])
         else run name expr params' ((copy_list params) :: env)
      | ScmLambda' (params', Opt opt, expr) ->
         let params' = params' @ [opt] in
         if (List.mem name params')
         then ([], [])
         else run name expr params' ((copy_list params) :: env)
      | ScmApplic' (proc, args, app_kind) ->
         let (rs1, ws1) = run name proc params env in
         let (rs2, ws2) = 
           combine_pairs
             (List.map
                (fun arg -> run name arg params env)
                args) in
         (rs1 @ rs2, ws1 @ ws2)
    in
    fun name expr params ->
    run name expr params [];;

  let cross_product as' bs' =
    List.concat (List.map (fun ai ->
                     List.map (fun bj -> (ai, bj)) bs')
                   as');;

  let should_box_var name expr params = raise X_not_yet_implemented;;

  let box_sets_and_gets name body =
    let rec run expr =
      match expr with
      | ScmConst' _ -> expr
      | ScmVarGet' (Var' (_, Free)) -> expr
      | ScmVarGet' (Var' (name', _) as v) ->
         if name = name'
         then ScmBoxGet' v
         else expr
      | ScmBox' _ -> expr
      | ScmBoxGet' _ -> expr
      | ScmBoxSet' (v, expr) -> ScmBoxSet' (v, run expr)
      | ScmIf' (test, dit, dif) ->
         ScmIf' (run test, run dit, run dif)
      | ScmSeq' exprs -> ScmSeq' (List.map run exprs)
      | ScmVarSet' (Var' (_, Free) as v, expr') ->
         ScmVarSet'(v, run expr')
      | ScmVarSet' (Var' (name', _) as v, expr') ->
         if name = name'
         then ScmBoxSet' (v, run expr')
         else ScmVarSet' (v, run expr')
      | ScmVarDef' (v, expr) -> ScmVarDef' (v, run expr)
      | ScmOr' exprs -> ScmOr' (List.map run exprs)
      | (ScmLambda' (params, Simple, expr)) as expr' ->
         if List.mem name params
         then expr'
         else ScmLambda' (params, Simple, run expr)
      | (ScmLambda' (params, Opt opt, expr)) as expr' ->
         if List.mem name (params @ [opt])
         then expr'
         else ScmLambda' (params, Opt opt, run expr)
      | ScmApplic' (proc, args, app_kind) ->
         ScmApplic' (run proc, List.map run args, app_kind)
    in
    run body;;

  let make_sets =
    let rec run minor names params =
      match names, params with
      | [], _ -> []
      | name :: names', param :: params' ->
         if name = param
         then let v = Var' (name, Param minor) in
              (ScmVarSet' (v, ScmBox' v)) :: (run (minor + 1) names' params')
         else run (minor + 1) names params'
      | _, _ -> raise (X_this_should_not_happen
                        "no free vars should be found here")
    in
    fun box_these params -> run 0 box_these params;;

  let rec auto_box expr =
    match expr with
    | ScmConst' _ | ScmVarGet' _ | ScmBox' _ | ScmBoxGet' _ -> expr
    | ScmBoxSet' (v, expr) -> ScmBoxSet' (v, auto_box expr)
    | ScmIf' (test, dit, dif) ->
       ScmIf' (auto_box test, auto_box dit, auto_box dif)
    | ScmSeq' exprs -> ScmSeq' (List.map auto_box exprs)
    | ScmVarSet' (v, expr) -> raise X_not_yet_implemented
    | ScmVarDef' (v, expr) -> raise X_not_yet_implemented
    | ScmOr' exprs -> raise X_not_yet_implemented
    | ScmLambda' (params, Simple, expr') ->
       let box_these =
         List.filter
           (fun param -> should_box_var param expr' params)
           params in
       let new_body = 
         List.fold_left
           (fun body name -> box_sets_and_gets name body)
           (auto_box expr')
           box_these in
       let new_sets = make_sets box_these params in
       let new_body = 
         match box_these, new_body with
         | [], _ -> new_body
         | _, ScmSeq' exprs -> ScmSeq' (new_sets @ exprs)
         | _, _ -> ScmSeq'(new_sets @ [new_body]) in
       ScmLambda' (params, Simple, new_body)
    | ScmLambda' (params, Opt opt, expr') ->
       raise X_not_yet_implemented
    | ScmApplic' (proc, args, app_kind) ->
       ScmApplic' (auto_box proc, List.map auto_box args, app_kind);;

  let semantics expr =
    auto_box
      (annotate_tail_calls
         (annotate_lexical_address expr));;

end;; (* end of module Semantic_Analysis *)

let sexpr_of_var' (Var' (name, _)) = ScmSymbol name;;

let rec sexpr_of_expr' = function
  | ScmConst' (ScmVoid) -> ScmVoid
  | ScmConst' ((ScmBoolean _) as sexpr) -> sexpr
  | ScmConst' ((ScmChar _) as sexpr) -> sexpr
  | ScmConst' ((ScmString _) as sexpr) -> sexpr
  | ScmConst' ((ScmNumber _) as sexpr) -> sexpr
  | ScmConst' ((ScmSymbol _) as sexpr) ->
     ScmPair (ScmSymbol "quote", ScmPair (sexpr, ScmNil))
  | ScmConst'(ScmNil as sexpr) ->
     ScmPair (ScmSymbol "quote", ScmPair (sexpr, ScmNil))
  | ScmConst' ((ScmVector _) as sexpr) ->
     ScmPair (ScmSymbol "quote", ScmPair (sexpr, ScmNil))      
  | ScmVarGet' var -> sexpr_of_var' var
  | ScmIf' (test, dit, ScmConst' ScmVoid) ->
     let test = sexpr_of_expr' test in
     let dit = sexpr_of_expr' dit in
     ScmPair (ScmSymbol "if", ScmPair (test, ScmPair (dit, ScmNil)))
  | ScmIf' (e1, e2, ScmConst' (ScmBoolean false)) ->
     let e1 = sexpr_of_expr' e1 in
     (match (sexpr_of_expr' e2) with
      | ScmPair (ScmSymbol "and", exprs) ->
         ScmPair (ScmSymbol "and", ScmPair(e1, exprs))
      | e2 -> ScmPair (ScmSymbol "and", ScmPair (e1, ScmPair (e2, ScmNil))))
  | ScmIf' (test, dit, dif) ->
     let test = sexpr_of_expr' test in
     let dit = sexpr_of_expr' dit in
     let dif = sexpr_of_expr' dif in
     ScmPair
       (ScmSymbol "if", ScmPair (test, ScmPair (dit, ScmPair (dif, ScmNil))))
  | ScmSeq' ([]) -> ScmVoid
  | ScmSeq' ([expr]) -> sexpr_of_expr' expr
  | ScmSeq' (exprs) ->
     ScmPair (ScmSymbol "begin", 
              Reader.scheme_sexpr_list_of_sexpr_list
                (List.map sexpr_of_expr' exprs))
  | ScmVarSet' (var, expr) ->
     let var = sexpr_of_var' var in
     let expr = sexpr_of_expr' expr in
     ScmPair (ScmSymbol "set!", ScmPair (var, ScmPair (expr, ScmNil)))
  | ScmVarDef' (var, expr) ->
     let var = sexpr_of_var' var in
     let expr = sexpr_of_expr' expr in
     ScmPair (ScmSymbol "define", ScmPair (var, ScmPair (expr, ScmNil)))
  | ScmLambda' (params, Simple, expr) ->
     let expr = sexpr_of_expr' expr in
     let params = Reader.scheme_sexpr_list_of_sexpr_list
                    (List.map (fun str -> ScmSymbol str) params) in
     ScmPair (ScmSymbol "lambda",
              ScmPair (params,
                       ScmPair (expr, ScmNil)))
  | ScmLambda' ([], Opt opt, expr) ->
     let expr = sexpr_of_expr' expr in
     let opt = ScmSymbol opt in
     ScmPair
       (ScmSymbol "lambda",
        ScmPair (opt, ScmPair (expr, ScmNil)))
  | ScmLambda' (params, Opt opt, expr) ->
     let expr = sexpr_of_expr' expr in
     let opt = ScmSymbol opt in
     let params = List.fold_right
                    (fun param sexpr -> ScmPair(ScmSymbol param, sexpr))
                    params
                    opt in
     ScmPair
       (ScmSymbol "lambda", ScmPair (params, ScmPair (expr, ScmNil)))
  | ScmApplic' (ScmLambda' (params, Simple, expr), args, app_kind) ->
     let ribs =
       Reader.scheme_sexpr_list_of_sexpr_list
         (List.map2
            (fun param arg -> ScmPair (ScmSymbol param, ScmPair (arg, ScmNil)))
            params
            (List.map sexpr_of_expr' args)) in
     let expr = sexpr_of_expr' expr in
     ScmPair
       (ScmSymbol "let",
        ScmPair (ribs,
                 ScmPair (expr, ScmNil)))
  | ScmApplic' (proc, args, app_kind) ->
     let proc = sexpr_of_expr' proc in
     let args =
       Reader.scheme_sexpr_list_of_sexpr_list
         (List.map sexpr_of_expr' args) in
     ScmPair (proc, args)
  | _ -> raise X_not_yet_implemented;;

let string_of_expr' expr =
  Printf.sprintf "%a" Reader.sprint_sexpr (sexpr_of_expr' expr);;

let print_expr' chan expr =
  output_string chan
    (string_of_expr' expr);;

let print_exprs' chan exprs =
  output_string chan
    (Printf.sprintf "[%s]"
       (String.concat "; "
          (List.map string_of_expr' exprs)));;

let sprint_expr' _ expr = string_of_expr' expr;;

let sprint_exprs' chan exprs =
  Printf.sprintf "[%s]"
    (String.concat "; "
       (List.map string_of_expr' exprs));;

(* end-of-input *)
 let print_sexpr = Reader.print_sexpr;;
 let print_sexprs = Reader.print_sexprs;;
 let sprint_sexpr = Reader.sprint_sexpr;;
 let sprint_sexprs = Reader.sprint_sexprs;;
 let scheme_sexpr_list_of_sexpr_list = Reader.scheme_sexpr_list_of_sexpr_list;;
 let nt_digit = Reader.nt_digit;;
 let nt_hex_digit = Reader.nt_hex_digit;;
 let nt_nat = Reader.nt_nat;;
 let is_decimal_digit = Reader.is_decimal_digit;;
 let nt_optional_sign = Reader.nt_optional_sign;;
 let nt_float = Reader.nt_float;;
 let nt_char_named = Reader.nt_char_named;;
 let nt_symbol = Reader.nt_symbol;;
 let nt_paired_comment = Reader.nt_paired_comment;;
 let nt_sexpr_comment = Reader.nt_sexpr_comment;;
 let nt_left_curly_bracket = Reader.nt_left_curly_bracket;;
 let nt_sexpr = Reader.nt_sexpr;;
let tag_parse = Tag_Parser.tag_parse;;
let macro_expand_cond_ribs = Tag_Parser.macro_expand_cond_ribs;;