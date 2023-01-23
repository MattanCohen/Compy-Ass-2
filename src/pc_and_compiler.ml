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
          (Printf.printf ";;; %s matchedTODO : FROM char %d to char %d, leaving %d chars unread\n"
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


(* pc.ml ends here *)


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

module Reader : READER = struct
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

module Tag_Parser : TAG_PARSER = struct
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
    | ScmNil -> ScmVoid
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


  let throw_and_print toPrint = 
    raise (X_syntax (string_of_sexpr(toPrint)));;
    (* match toPrint with *)
    (* | sexpr -> print_sexpr(toPrint);; *)
    (* | sexpr -> raise (X_syntax (string_of_sexpr(toPrint)));; *)
    (* | expr -> print_expr(toPrint);; *)
    (* | expr -> raise (X_syntax (print_expr(toPrint)));; *)


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



  let rec tag_parse sexpr =
   
    let rec macro_expand_let_ribs : sexpr -> sexpr = fun ribs ->
      match ribs with  
        | ScmNil -> ScmNil
        | ScmPair(ScmPair(argName, argVal), exprs) -> ScmPair(argName, macro_expand_let_ribs exprs)
        | _ -> raise (X_syntax "Bad let ribs")  in

    let rec macro_expand_let_args : sexpr -> sexpr = fun ribs ->
      match ribs with  
      | ScmNil -> ScmNil
      | ScmPair(ScmPair(argName, ScmPair(argb, ScmNil)), exprs) -> ScmPair(argb, macro_expand_let_args exprs)
        | _ -> raise (X_syntax "Bad let ribs args") in
      
    
    let macro_expand_let ribs exprs =
        let args = macro_expand_let_ribs ribs in
        let modExprs = macro_expand_let_args ribs in
        tag_parse (ScmPair(ScmPair(ScmSymbol "lambda", ScmPair(args, exprs)), modExprs)) in

        
  let rec macro_expand_let_star vars body =
    let rec handle_pairs = function
        | [] -> (ScmNil, ScmNil)
        | ScmPair(id, ScmPair(expr, ScmNil))::rest ->
          let (r_id, r_expr) = handle_pairs rest in
          (ScmPair(id,r_id), ScmPair(expr,r_expr))
        | _ -> raise (X_syntax "malformed let expr") in
      let v_id, v_expr = handle_pairs vars in
      ScmPair
      (ScmPair
        (ScmSymbol "lambda",
        ScmPair (v_id, body)),
      v_expr) in


  
  let rec macro_expand_letrec vars body =
      let whatever = ScmPair (ScmSymbol "quote", ScmPair (ScmSymbol "whatever", ScmNil)) in
        let rec fix_let_rec = function
          | [] -> (ScmNil, body)
          | ScmPair(id, sexpr)::rest ->
            let (r_init, r_set) = fix_let_rec rest in
            (ScmPair(ScmPair(id, ScmPair(whatever, ScmNil)), r_init),
            ScmPair(ScmPair(ScmSymbol("set!"),
                            ScmPair(id, sexpr)), r_set))
          | _ -> raise (X_syntax "malformed let expr") in
        let inits, sets = fix_let_rec vars in
        ScmPair (ScmSymbol "let", ScmPair (inits, sets)) in


    let rec run = fun sexpression -> match sexpression with
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
        
    | ScmPair (ScmSymbol "let", ScmPair (ribs, exprs)) -> macro_expand_let ribs exprs
    | ScmPair (ScmSymbol "let*", ScmPair (ScmNil, exprs)) ->  macro_expand_let ScmNil exprs
    | ScmPair (ScmSymbol "let*",
               ScmPair
                 (ScmPair
                    (ScmPair (var, ScmPair (value, ScmNil)), ScmNil),
                  exprs)) ->
                    let ribs = ScmPair(var, ScmPair (value, ScmNil)) in
                    let ribs = ScmPair(ribs, ScmNil) in
                    macro_expand_let ribs exprs
    | ScmPair (ScmSymbol "let*",
               ScmPair (ScmPair (ScmPair (var,
                                          ScmPair (arg, ScmNil)),
                                 ribs),
                        exprs)) ->
          let rest = ScmPair(ScmPair(ScmSymbol "let*", ScmPair(ribs, exprs)), ScmNil) in
          tag_parse (macro_expand_let_star [ScmPair (var, ScmPair (arg, ScmNil))] rest)
    
    | ScmPair (ScmSymbol "letrec", ScmNil) ->
            raise (X_syntax "invalid letrec param list")
    
    | ScmPair (ScmSymbol "letrec", ScmPair (vars, body)) ->
            (match (scheme_list_to_ocaml vars) with
            | vars, ScmNil ->
                tag_parse (macro_expand_letrec vars body)
            | _ -> raise (X_syntax "invalid letrec param list"))

    | ScmPair (ScmSymbol "and", ScmNil) -> ScmConst (ScmBoolean true)
    | ScmPair (ScmSymbol "and", exprs) ->
      (match (scheme_list_to_ocaml exprs) with
      | expr :: exprs, ScmNil ->
         tag_parse (macro_expand_and_clauses expr exprs)
      | _ -> raise (X_syntax "malformed and-expression"))
    | ScmPair (ScmSymbol "or", ScmNil) -> ScmConst (ScmBoolean(false))
    | ScmPair (ScmSymbol "or", ScmPair(expr, ScmNil)) -> tag_parse expr
    | ScmPair (ScmSymbol "or", exprs) -> 
      (match (scheme_list_to_ocaml exprs) with
      | (sexprs', ScmNil) -> ScmOr(List.map tag_parse sexprs')
      | _ -> raise (X_syntax "malformed or-expr"))
    | ScmPair (ScmSymbol "cond", ribs) ->
       tag_parse (macro_expand_cond_ribs ribs)
    | ScmPair (proc, args) ->
       let proc =
         (match proc with
          | ScmSymbol var ->
             if (is_reserved_word var)
             then raise (X_syntax (Printf.sprintf "reserved word in proc position: %s\nsexp is: %s" var (string_of_sexpr sexpression)))
             else proc
          | proc -> proc) in
       (match (scheme_list_to_ocaml args) with
        | args, ScmNil ->
           ScmApplic (tag_parse proc, List.map tag_parse args)
        | _ -> raise (X_syntax "malformed application"))
    | sexpr -> raise (X_syntax
                       (Printf.sprintf
                          "Unknown form in tag parser: \n%a\n"
                          Reader.sprint_sexpr sexpr)) in
    
    run sexpr;;

  
  
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
      | ScmConst sexpr -> ScmConst' sexpr
      | ScmVarGet (Var str) -> ScmVarGet' (tag_lexical_address_for_var str params env)
      | ScmIf (test, dit, dif) ->  ScmIf' (run test params env, run dit params env,run dif params env)
      | ScmSeq exprs ->  ScmSeq' (List.map (fun expr -> run expr params env) exprs)
      | ScmOr exprs -> ScmOr' (List.map (fun expr -> run expr params env) exprs)
      | ScmVarSet(Var v, expr) -> ScmVarSet' ((tag_lexical_address_for_var v params env),
          (run expr params env))
      (* this code does not [yet?] support nested define-expressions *)
      | ScmVarDef(Var v, expr) -> ScmVarDef' ((Var' (v, Free)), (run expr params env))
      | ScmLambda (params', Simple, expr) ->  ScmLambda' (params', Simple, run expr params' (List.append [params] env))
      | ScmLambda (params', Opt opt, expr) -> 
        ScmLambda' (params', Opt opt, run expr (List.append params' [opt]) (List.append [params] env))
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
      | (ScmConst' _) as orig -> orig
      | (ScmVarGet' _) as orig -> orig
      | ScmIf' (test, dit, dif) -> ScmIf' (run false test, run in_tail dit, run in_tail dif)
      | ScmSeq' [] -> ScmSeq' []
      | ScmSeq' (expr :: exprs) -> ScmSeq' (runl in_tail expr exprs)
      | ScmOr' [] ->  ScmOr' []
      | ScmOr' (expr :: exprs) -> ScmOr' (runl in_tail expr exprs)
      | ScmVarSet' (var', expr') -> ScmVarSet' (var', run false expr')
      | ScmVarDef' (var', expr') -> ScmVarDef' (var', run false expr')
      | (ScmBox' _) as expr' -> expr'
      | (ScmBoxGet' _) as expr' -> expr'
      | ScmBoxSet' (var', expr') -> ScmBoxSet' (var', run false expr')
      | ScmLambda' (params, Simple, expr) -> ScmLambda' (params, Simple, run true expr)
      | ScmLambda' (params, Opt opt, expr) -> ScmLambda' (params, Opt opt, run true expr)
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
    fun expr' -> run false expr';;

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

  let should_box_var name expr params =
    let read, write = find_reads_and_writes name expr params in
    let same_pair = List.map (fun pair -> match pair with
    | ((Var' (_, Param _), _), (Var' (_, Param _), _)) -> true
    | ((Var' (_, Param _), _), _) | (_, (Var' (_, Param _), _)) -> false
    | ((Var' (_, Bound (rm,_)), re),
      (Var' (_, Bound (wm,_)), we)) ->
        (List.nth re rm) == (List.nth we wm)
    | _ -> raise (X_this_should_not_happen "Should't happen!")) (cross_product read write) in
    List.exists (fun x -> x == false) same_pair
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
    | ScmVarSet' (v, expr) -> ScmVarSet' (v, auto_box expr)
    | ScmVarDef' (v, expr) -> ScmVarDef' (v, auto_box expr)
    | ScmOr' exprs -> ScmOr' (List.map auto_box exprs)
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
      let all_params' = [opt] @ params in
      let box_these =
        List.filter
          (fun param -> should_box_var param expr' all_params')
          all_params' in
      let new_body = 
        List.fold_left
          (fun body name -> box_sets_and_gets name body)
          (auto_box expr')
          box_these in
      let new_sets = make_sets box_these params in
      let new_body = match box_these, new_body with
        | [], _ -> new_body
        | _, ScmSeq' exprs -> ScmSeq' (new_sets @ exprs)
        | _, _ -> ScmSeq'(new_sets @ [new_body]) in
      ScmLambda' (params, Opt opt, new_body)
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
  | _ -> raise (X_this_should_not_happen "not expr'");;

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



let file_to_string input_file =
  let in_channel = open_in input_file in
  let rec run () =
    try 
      let ch = input_char in_channel in ch :: (run ())
    with End_of_file ->
      ( close_in in_channel;
	[] )
  in string_of_list (run ());;

let string_to_file output_file out_string =
  let out_channel = open_out output_file in
  ( output_string out_channel out_string;
    close_out out_channel );;

module type CODE_GENERATION =
  sig
    val compile_scheme_string : string -> string -> unit
    val compile_scheme_file : string -> string -> unit
  end;;



module Code_Generation : CODE_GENERATION = struct

  (* areas that raise this exception are NOT for the
   * final project! please leave these unimplemented,
   * as this will require major additions to your
   * compilers
   *)
  exception X_not_yet_supported;;

  let word_size = 8;;
  let label_start_of_constants_table = "L_constants";;
  let comment_length = 20;;

  let list_and_last =
    let rec run a = function
      | [] -> ([], a)
      | b :: s ->
         let (s, last) = run b s in
         (a :: s, last)
    in function
    | [] -> None
    | a :: s -> Some (run a s);;

  let split_to_sublists n = 
    let rec run = function
      | ([], _, f) -> [f []]
      | (s, 0, f) -> (f []) :: (run (s, n, (fun s -> s)))
      | (a :: s, i, f) ->
         (run (s, i - 1, (fun s -> f (a :: s))))
    in function
    | [] -> []
    | s -> run (s, n, (fun s -> s));;
  
  let foreach: ('a -> unit) -> 'a list -> unit = fun func list ->
    List.fold_left (fun _ x -> func x) () list;;

  let remove_duplicates list =
    let res =  ref [] in
    let add_if_not_member x = if List.mem x res.contents then () else res := res.contents @ [x] in
    let _ = foreach add_if_not_member list in
    res.contents

  (* TODO: TEST *)
  let collect_constants exprs' =
    let rec runs: expr' list -> sexpr list = fun exprs' -> List.flatten (List.map run exprs')
      and run: expr' -> sexpr list = fun expr' ->
        match expr' with
          | ScmConst' sexpr -> [sexpr]
          | ScmVarGet' _ 
          | ScmVarSet' _ 
          | ScmBox' _ 
          | ScmBoxGet' _ -> []
          | ScmIf' (test, dit, dif) ->  runs [test; dit; dif]
          | ScmSeq' exprs'
          | ScmOr' exprs' -> runs exprs'
          | ScmVarDef' (_, expr')
          | ScmBoxSet' (_, expr') -> run expr'
          | ScmLambda' (_, _, expr') -> run expr'
          | ScmApplic' (expr', exprs', _) -> runs (expr' :: exprs') in
  runs exprs';;

  (* TODO: TEST *)
  let add_sub_constants =
    let rec run sexpr = match sexpr with
      | ScmVoid | ScmNil | ScmBoolean _ | ScmChar _ | ScmString _ | ScmNumber _ -> [sexpr]
      | ScmSymbol sym -> [ScmString(sym) ; sexpr]
      | ScmPair (car, cdr) -> (run car) @ (run cdr) @ [sexpr]
      | ScmVector sexprs -> runs sexprs @ [sexpr]
    and runs sexprs =
      List.fold_left (fun full sexpr -> full @ (run sexpr)) [] sexprs
    in fun exprs' ->
       [ScmVoid; ScmNil; ScmBoolean false; ScmBoolean true; ScmChar '\000'] @ (runs exprs');;

  type initialized_data =
    | RTTI of string
    | Byte of int
    | ASCII of string
    | Quad of int
    | QuadFloat of float
    | ConstPtr of int;;
  
  type constant_entry = sexpr * int * initialized_data list;;
  let get_sexp: constant_entry -> sexpr = fun entry -> match entry with
      (sexp, _ , _) -> sexp;;
  let get_address: constant_entry -> int = fun entry -> match entry with
      (_, address, _) -> address;;
  let get_initialized_data: constant_entry -> initialized_data list = fun entry -> match entry with
      (_, _, data) -> data;;

  type constants_table = constant_entry list;;

  (* TODO: TEST *)
  let search_constant_address: sexpr -> constants_table -> int = fun sym table ->
    let entry = List.find (fun entry -> get_sexp entry = sym) table in
    get_address entry;;

  let const_repr sexpr loc table = match sexpr with
    | ScmVoid -> ([RTTI "T_void"], 1)
    | ScmNil -> ([RTTI "T_nil"], 1)
    | ScmBoolean false ->
       ([RTTI "T_boolean_false"], 1)
    | ScmBoolean true ->
       ([RTTI "T_boolean_true"], 1)
    | ScmChar ch ->
       ([RTTI "T_char"; Byte (int_of_char ch)], 2)
    | ScmString str ->
       let count = String.length str in
       ([RTTI "T_string"; Quad count; ASCII str],
        1 + word_size + count)
    | ScmSymbol sym ->
       let addr = search_constant_address (ScmString sym) table in
       ([RTTI "T_symbol"; ConstPtr addr], 1 + word_size)
    | ScmNumber (ScmRational (numerator, denominator)) ->
       ([RTTI "T_rational"; Quad numerator; Quad denominator],
        1 + 2 * word_size)
    | ScmNumber (ScmReal x) ->
       ([RTTI "T_real"; QuadFloat x], 1 + word_size)
    | ScmVector s ->
       let addrs =
         List.map
           (fun si -> ConstPtr (search_constant_address si table)) s in
       let count = List.length s in
       ((RTTI "T_vector") :: (Quad count) :: addrs,
        1 + (count + 1) * word_size)
    | ScmPair (car, cdr) ->
       let (addr_car, addr_cdr) =
         (search_constant_address car table,
          search_constant_address cdr table) in
       ([RTTI "T_pair"; ConstPtr addr_car; ConstPtr addr_cdr],
        1 + 2 * word_size);;

  let make_constants_table =
    let rec run table loc = function
      | [] -> table
      | sexpr :: sexprs ->
         let (repr, len) = const_repr sexpr loc table in
         run (table @ [(sexpr, loc, repr)]) (loc + len) sexprs
    in
    fun exprs' ->
    run [] 0
      (remove_duplicates
         (add_sub_constants
            (remove_duplicates
               (collect_constants exprs'))));;    

  let asm_comment_of_sexpr sexpr =
    let str = string_of_sexpr sexpr in
    let str =
      if (String.length str) <= comment_length
      then str
      else (String.sub str 0 comment_length) ^ "..." in
    "; " ^ str;;

  let asm_of_representation sexpr =
    let str = asm_comment_of_sexpr sexpr in
    let run = function
      | [RTTI str] -> Printf.sprintf "\tdb %s" str
      | [RTTI "T_char"; Byte byte] ->
         Printf.sprintf "\tdb T_char, 0x%02X\t%s" byte str
      | [RTTI "T_string"; Quad length; ASCII const_str] ->
         Printf.sprintf "\tdb T_string\t%s\n\tdq %d%s"
           str length
           (let s = list_of_string const_str in
            let s = List.map
                      (fun ch -> Printf.sprintf "0x%02X" (int_of_char ch))
                      s in
            let s = split_to_sublists 8 s in
            let s = List.map (fun si -> "\n\tdb " ^ (String.concat ", " si)) s in
            String.concat "" s)
      | [RTTI "T_symbol"; ConstPtr addr] ->
         Printf.sprintf "\tdb T_symbol\t%s\n\tdq %s + %d"
           str label_start_of_constants_table addr
      | [RTTI "T_rational"; Quad numerator; Quad denominator] ->
         Printf.sprintf "\tdb T_rational\t%s\n\tdq %d, %d"
           str
           numerator denominator
      | [RTTI "T_real"; QuadFloat x] ->
         Printf.sprintf "\tdb T_real\t%s\n\tdq %f" str x
      | (RTTI "T_vector") :: (Quad length) :: addrs ->
         Printf.sprintf "\tdb T_vector\t%s\n\tdq %d%s"
           str length
           (let s = List.map
                      (function
                       | ConstPtr ptr ->
                          Printf.sprintf "%s + %d"
                            label_start_of_constants_table ptr
                       | _ -> raise
                               (X_this_should_not_happen
                                  "incorrect representation for a vector"))
                      addrs in
            let s = split_to_sublists 3 s in
            let s = List.map (fun si -> "\n\tdq " ^ (String.concat ", " si)) s in
            String.concat "" s)
      | [RTTI "T_pair"; ConstPtr car; ConstPtr cdr] ->
         Printf.sprintf "\tdb T_pair\t%s\n\tdq %s + %d, %s + %d"
           str
           label_start_of_constants_table car
           label_start_of_constants_table cdr
      | _ -> raise (X_this_should_not_happen "invalid representation!")
    in run;;

  let asm_of_constants_table =
    let rec run = function
      | [] -> ""
      | (sexpr, _, repr) :: rest ->
         (asm_of_representation sexpr repr) ^ "\n" ^ (run rest)
    in
    fun table ->
    Printf.sprintf "%s:\n%s"
      label_start_of_constants_table (run table);;

  let global_bindings_table =
    [ (* 1-10 *)
      ("null?", "L_code_ptr_is_null");
      ("pair?", "L_code_ptr_is_pair");
      ("void?", "L_code_ptr_is_void");
      ("char?", "L_code_ptr_is_char");
      ("string?", "L_code_ptr_is_string");
      ("symbol?", "L_code_ptr_is_symbol");
      ("vector?", "L_code_ptr_is_vector");
      ("procedure?", "L_code_ptr_is_closure");
      ("real?", "L_code_ptr_is_real");
      ("rational?", "L_code_ptr_is_rational");
      ("boolean?", "L_code_ptr_is_boolean");
      (* 11-20 *)
      ("number?", "L_code_ptr_is_number");
      ("collection?", "L_code_ptr_is_collection");
      ("cons", "L_code_ptr_cons");
      ("display-sexpr", "L_code_ptr_display_sexpr");
      ("write-char", "L_code_ptr_write_char");
      ("car", "L_code_ptr_car");
      ("cdr", "L_code_ptr_cdr");
      ("string-length", "L_code_ptr_string_length");
      ("vector-length", "L_code_ptr_vector_length");
      ("real->integer", "L_code_ptr_real_to_integer");
      (* 21-30*)
      ("exit", "L_code_ptr_exit");
      ("integer->real", "L_code_ptr_integer_to_real");
      ("rational->real", "L_code_ptr_rational_to_real");
      ("char->integer", "L_code_ptr_char_to_integer");
      ("integer->char", "L_code_ptr_integer_to_char");
      ("trng", "L_code_ptr_trng");
      ("zero?", "L_code_ptr_is_zero");
      ("integer?", "L_code_ptr_is_integer");
      ("__bin-apply", "L_code_ptr_bin_apply");
      ("__bin-add-rr", "L_code_ptr_raw_bin_add_rr");
      (* 31-40*)
      ("__bin-sub-rr", "L_code_ptr_raw_bin_sub_rr");
      ("__bin-mul-rr", "L_code_ptr_raw_bin_mul_rr");
      ("__bin-div-rr", "L_code_ptr_raw_bin_div_rr");
      ("__bin-add-qq", "L_code_ptr_raw_bin_add_qq");
      ("__bin-sub-qq", "L_code_ptr_raw_bin_sub_qq");
      ("__bin-mul-qq", "L_code_ptr_raw_bin_mul_qq");
      ("__bin-div-qq", "L_code_ptr_raw_bin_div_qq");
      ("error", "L_code_ptr_error");
      ("__bin-less-than-rr", "L_code_ptr_raw_less_than_rr");
      ("__bin-less-than-qq", "L_code_ptr_raw_less_than_qq");
      (* 41-50 *)
      ("__bin-equal-rr", "L_code_ptr_raw_equal_rr");
      ("__bin-equal-qq", "L_code_ptr_raw_equal_qq");
      ("quotient", "L_code_ptr_quotient");
      ("remainder", "L_code_ptr_remainder");
      ("set-car!", "L_code_ptr_set_car");
      ("set-cdr!", "L_code_ptr_set_cdr");
      ("string-ref", "L_code_ptr_string_ref");
      ("vector-ref", "L_code_ptr_vector_ref");
      ("vector-set!", "L_code_ptr_vector_set");
      ("string-set!", "L_code_ptr_string_set");
      (* 51-60 *)
      ("make-vector", "L_code_ptr_make_vector");
      ("make-string", "L_code_ptr_make_string");
      ("numerator", "L_code_ptr_numerator");
      ("denominator", "L_code_ptr_denominator");
      ("eq?", "L_code_ptr_eq")
    ];;

  
  (*TODO: TEST*)
  let collect_free_vars =
    let rec run = function
      | ScmConst' _ -> []
      | ScmVarGet' (Var' (v, Free)) -> [v]
      | ScmVarGet' _ -> []
      | ScmIf' (test, dit, dif) -> runs [test; dit; dif]
      | ScmSeq' exprs' -> runs exprs'
      | ScmOr' exprs' -> runs exprs'
      | ScmVarSet' (Var' (v, Free), expr') -> v :: run expr'
      | ScmVarSet' (_, expr') -> run expr'
      | ScmVarDef' (Var' (v, Free), expr') -> v :: run expr'
      | ScmVarDef' (_, expr') -> run expr'
      | ScmBox' (Var' (v, Free)) -> [v]
      | ScmBox' _ -> []
      | ScmBoxGet' (Var' (v, Free)) -> [v]
      | ScmBoxGet' _ -> []
      | ScmBoxSet' (Var' (v, Free), expr') -> v :: run expr'
      | ScmBoxSet' (_, expr') -> run expr'
      | ScmLambda' (_, _, expr') -> run expr'
      | ScmApplic' (expr', exprs', _) -> runs (expr' :: exprs')
    and runs exprs' =
      List.fold_left
        (fun vars expr' -> vars @ (run expr'))
        []
        exprs'
    in fun exprs' ->
       let primitives =
         List.map
           (fun (scheme_name, _) -> scheme_name)
           global_bindings_table
       and free_vars_in_code = runs exprs' in
       remove_duplicates
         (primitives @ free_vars_in_code);;

  let make_free_vars_table =
    let rec run index = function
      | [] -> []
      | v :: vars ->
         let x86_label = Printf.sprintf "free_var_%d" index in
         (v, x86_label) :: (run (index + 1) vars)
    in fun exprs' -> run 0 (collect_free_vars exprs');;

  let search_free_var_table =
    let rec run v = function
      | [] -> raise (X_this_should_not_happen
                      (Printf.sprintf
                         "The variable %s was not found in the free-var table"
                         v))
      | (v', x86_label) :: _ when v = v' -> x86_label
      | _ :: table -> run v table
    in run;;

  let asm_of_global_bindings global_bindings_table free_var_table =
    String.concat "\n"
      (List.map
         (fun (scheme_name, asm_code_ptr) ->
           let free_var_label =
             search_free_var_table scheme_name free_var_table in
           (Printf.sprintf "\t; building closure for %s\n" scheme_name)
           ^ (Printf.sprintf "\tmov rdi, %s\n" free_var_label)
           ^ (Printf.sprintf "\tmov rsi, %s\n" asm_code_ptr)
           ^ "\tcall bind_primitive\n")
         global_bindings_table);;
  
  let asm_of_free_vars_table table =
    let tmp = 
      List.map
        (fun (scm_var, asm_label) ->
          Printf.sprintf "%s:\t; location of %s\n\tresq 1"
            asm_label scm_var)
        table in
    String.concat "\n" tmp;;

  let make_make_label prefix =
    let index = ref 0 in
    fun () ->
    (index := !index + 1;
     Printf.sprintf "%s_%04x" prefix !index);;

  let make_if_else = make_make_label ".L_if_else";;
  let make_if_end = make_make_label ".L_if_end";;
  let make_or_end = make_make_label ".L_or_end";;
  let make_lambda_simple_loop_env =
    make_make_label ".L_lambda_simple_env_loop";;
  let make_lambda_simple_loop_env_end =
    make_make_label ".L_lambda_simple_env_end";;
  let make_lambda_simple_loop_params =
    make_make_label ".L_lambda_simple_params_loop";;
  let make_lambda_simple_loop_params_end =
    make_make_label ".L_lambda_simple_params_end";;
  let make_lambda_simple_code = make_make_label ".L_lambda_simple_code";;
  let make_lambda_simple_end = make_make_label ".L_lambda_simple_end";;
  let make_lambda_simple_arity_ok =
    make_make_label ".L_lambda_simple_arity_check_ok";;
  let make_lambda_opt_loop_env =
    make_make_label ".L_lambda_opt_env_loop";;
  let make_lambda_opt_loop_env_end =
    make_make_label ".L_lambda_opt_env_end";;
  let make_lambda_opt_loop_params =
    make_make_label ".L_lambda_opt_params_loop";;
  let make_lambda_opt_loop_params_end =
    make_make_label ".L_lambda_opt_params_end";;
  let make_lambda_opt_code = make_make_label ".L_lambda_opt_code";;
  let make_lambda_opt_end = make_make_label ".L_lambda_opt_end";;
  let make_lambda_opt_arity_exact =
    make_make_label ".L_lambda_opt_arity_check_exact";;
  let make_lambda_opt_arity_more =
    make_make_label ".L_lambda_opt_arity_check_more";;
  let make_lambda_opt_stack_ok =
    make_make_label ".L_lambda_opt_stack_adjusted";;
  let make_lambda_opt_loop =
    make_make_label ".L_lambda_opt_stack_shrink_loop";;
  let make_lambda_opt_loop_exit =
    make_make_label ".L_lambda_opt_stack_shrink_loop_exit";;
  let make_tc_applic_recycle_frame_loop =
    make_make_label ".L_tc_recycle_frame_loop";;
  let make_tc_applic_recycle_frame_done =
    make_make_label ".L_tc_recycle_frame_done";;
  (*let get_sexp: constant_entry -> sexpr = fun entry -> match entry with
      (sexp, _ , _) -> sexp;;
  let get_address: constant_entry -> int = fun entry -> match entry with
      (_, address, _) -> address;;*)
  let rec const_table_to_string: (constants_table -> string) = function
    | [] -> ""
    | first :: rest -> (Printf.sprintf "sexp: %s, address: %d\n%s" 
      (string_of_sexpr (get_sexp first))
      (get_address first) 
      (const_table_to_string rest))
    

  (*TODO: IMPLEMENT*)
  let rec code_gen exprs' =
    let consts = make_constants_table exprs' in
    let _ = print_endline (const_table_to_string consts) in
    let free_vars = make_free_vars_table exprs' in
    let rec run params env = function
      | ScmConst' sexpr -> (*DONE : FROM chapter 6 slides: page 76 *)
        let address = search_constant_address sexpr consts in
        Printf.sprintf
          "\tmov rax, %s + %d\n"
          label_start_of_constants_table
          address
      | ScmVarGet' (Var' (v, Free)) -> (* WRITTEN BY MAIER! -TODO : FROM chapter 6 slides: page 81 *)
         let label = search_free_var_table v free_vars in
         Printf.sprintf
           "\tmov rax, qword [%s]\n"
           label
      | ScmVarGet' (Var' (v, Param minor)) -> (*DONE : FROM chapter 6 slides: page 77 *)
          Printf.sprintf
          "\tmov rax, qword [rbp + 8 * %d]\n"
          (minor + 4)
      | ScmVarGet' (Var' (v, Bound (major, minor))) -> (*DONE MATTAN : FROM chapter 6 slides: page 79 *)
         "\t; performing var get\n"
         ^ "\tmov rax, qword [rbp + 8 * 2]\n"
         ^ (Printf.sprintf "\tmov rax, qword [rbp + 8 * %d]\n" major)
         ^ (Printf.sprintf "\tmov rax, qword [rbp + 8 * %d]\n" minor)

      | ScmIf' (test, dit, dif) -> (*DONE MATTAN : FROM chapter 6 slides: page 86 *)
        let genedTest = (run params env test) in 
        let genedDit = (run params env dit) in 
        let genedDif = (run params env dif) in 
        let elseLabel = make_if_else() in
        let exitLabel = make_if_end() in
        "\t; performing if statement\n"
        ^ Printf.sprintf "%s\n" genedTest
        ^ "\tcmp rax, sob_boolean_false\n"
        ^ (Printf.sprintf "\tje %s\n" elseLabel)
        ^ Printf.sprintf "%s\n" genedDit
        ^ (Printf.sprintf "\tjmp %s\n" exitLabel)
        ^ (Printf.sprintf "%s:\n" elseLabel)
        ^ Printf.sprintf "%s\n" genedDif
        ^ (Printf.sprintf "%s:\n" exitLabel)
   | ScmSeq' exprs' -> 
         String.concat "\n"
           (List.map (run params env) exprs')
      | ScmOr' exprs' ->
         let label_end = make_or_end () in
         let asm_code = 
           (match (list_and_last exprs') with
            | Some (exprs', last_expr') ->
               let exprs_code =
                 String.concat ""
                   (List.map
                      (fun expr' ->
                        let expr_code = run params env expr' in
                        expr_code
                        ^ "\tcmp rax, sob_boolean_false\n"
                        ^ (Printf.sprintf "\tjne %s\n" label_end))
                      exprs') in
               let last_expr_code = run params env last_expr' in
               exprs_code
               ^ last_expr_code
               ^ (Printf.sprintf "%s:\n" label_end)
            (* and just in case someone messed up the tag-parser: *)
            | None -> run params env (ScmConst' (ScmBoolean false)))
         in asm_code
      | ScmVarSet' (Var' (v, Free), expr') -> (*DONE Mattan : FROM chapter 6 slides: page 82 *)
          let genedExpr = (run params env expr') in
          let labelInFVarTableV = search_free_var_table v free_vars in 
          "\t; performing free var set statement\n"
          ^ Printf.sprintf "%s\n" genedExpr
          ^ (Printf.sprintf "\tmov qword [%s], rax \n" labelInFVarTableV)
          ^ "\tmov rax, sob_void\n"
      | ScmVarSet' (Var' (v, Param minor), expr') -> (*DONE Mattan : FROM chapter 6 slides: page 78 *)
          let genedExpr = (run params env expr') in 
          "\t;performing var set statement param\n"
          ^ Printf.sprintf "%s\n" genedExpr
          ^ (Printf.sprintf "\tmov qword [rbp + 8 * (4 + %d)], rax \n" minor)
          ^ "\tmov rax, sob_void\n"
      | ScmVarSet' (Var' (v, Bound (major, minor)), expr') -> (*DONE Mattan : FROM chapter 6 slides: page 80 *)
          let genedExpr = (run params env expr') in 
          "\t;performing var set statement bound\n"
          ^ Printf.sprintf "%s\n" genedExpr
          ^ "\tmov rbx, qword [rbp + 8 * 2]\n"
          ^ (Printf.sprintf "\tmov rbx, qword [rbp + 8 * %d]\n" major)
          ^ (Printf.sprintf "\tmov qword [rbp + 8 * %d], rax \n" minor)
          ^ "\tmov rax, sob_void\n"
      | ScmVarDef' (Var' (v, Free), expr') -> 
         let label = search_free_var_table v free_vars in
         (run params env expr')
         ^ (Printf.sprintf "\tmov qword [%s], rax\n" label)
         ^ "\tmov rax, sob_void\n"
      | ScmVarDef' (Var' (v, Param minor), expr') ->
         raise X_not_yet_supported
      | ScmVarDef' (Var' (v, Bound (major, minor)), expr') ->
         raise X_not_yet_supported
      | ScmBox' (Var' (v, Param minor)) -> (*DONE MATTAN : FROM chapter 6 slides: page 89uz *)
        "\t mov rdi, 8 \n"
        (* "" *)
        ^ "\t call malloc ; call malloc (rax will contain empty box)\n"
        ^ Printf.sprintf "\t mov rbx, PARAM(%d)\n ; set rbx <- [param(minor)]" minor
        ^ "\t mov [rax], rbx ; push in malloc's dedicated location value of compied rbx\n"
        ^ Printf.sprintf "\t mov PARAM(%d), rax ; put the box with the contains in param minor\n" minor
        ^ "\t mov rax, sob_void ; set rax as void since its setbox\n"
        | ScmBox' _ -> raise (X_this_should_not_happen "ScmBox with nothing should not happen")
      | ScmBoxGet' var' ->
         (run params env (ScmVarGet' var'))
         ^ "\tmov rax, qword [rax]\n"
      | ScmBoxSet' (var', expr') -> (*DONE MATTAN : FROM chapter 6 slides: page 90 *)
        let genedExpr = (run params env expr') in 
        let genedVar = (run params env (ScmVarGet' var')) in 
        "\t; performing box set statement\n"
        ^ Printf.sprintf "%s\n" genedExpr
        ^ "\tpush rax\n"
        ^ Printf.sprintf "%s\n" genedVar
        ^ "\tpop qword[rax]\n"
        ^ "\tmov rax, sob_void\n"
      | ScmLambda' (params', Simple, body) -> (* WRITTEN BY MAIER! -TODO : FROM chapter 6 slides: page 91 *)
         let label_loop_env = make_lambda_simple_loop_env ()
         and label_loop_env_end = make_lambda_simple_loop_env_end ()
         and label_loop_params = make_lambda_simple_loop_params ()
         and label_loop_params_end = make_lambda_simple_loop_params_end ()
         and label_code = make_lambda_simple_code ()
         and label_arity_ok = make_lambda_simple_arity_ok ()
         and label_end = make_lambda_simple_end ()
         in
         
         (*  allocate closure object *)
         "\tmov rdi, (1 + 8 + 8)\t; lambda simple : sob closure\n"
         ^ "\tcall malloc\n"
         ^ "\tpush rax\n"

         (* create ExtEnv *)
         (* create new rib for new env *)
         ^ (Printf.sprintf "\tmov rdi, 8 * %d\t; lambda simple : new rib\n" (List.length params'))
         ^ "\tcall malloc\n"
         ^ "\tpush rax\n"
         (* copy pointers *)
         ^ (Printf.sprintf "\tmov rdi, 8 * %d\t; lambda simple : extended env\n" (env + 1))
         ^ "\tcall malloc\n"
         ^ "\tmov rdi, ENV\n"
         ^ "\tmov rsi, 0\n"
         ^ "\tmov rdx, 1\n"
         ^ (Printf.sprintf "%s:\t; lambda simple : ext_env[i + 1] <-- env[i]\n"
              label_loop_env)
         ^ (Printf.sprintf "\tcmp rsi, %d\n" env)
         ^ (Printf.sprintf "\tje %s\n" label_loop_env_end)
         ^ "\tmov rcx, qword [rdi + 8 * rsi]\n"
         ^ "\tmov qword [rax + 8 * rdx], rcx\n"
         ^ "\tinc rsi\n"
         ^ "\tinc rdx\n"
         ^ (Printf.sprintf "\tjmp %s\n" label_loop_env)
         (* end of copy pointers *)
         ^ (Printf.sprintf "%s:\n" label_loop_env_end)
         ^ "\tpop rbx\n"
         ^ "\tmov rsi, 0\n"
         (* copy parameters off of the stack *)
         ^ (Printf.sprintf "%s:\t; lambda simple : copy params\n" label_loop_params)
         ^ (Printf.sprintf "\tcmp rsi, %d\n" params)
         ^ (Printf.sprintf "\tje %s\n" label_loop_params_end)
         ^ "\tmov rdx, qword [rbp + 8 * rsi + 8 * 4]\n"
         ^ "\tmov qword [rbx + 8 * rsi], rdx\n"
         ^ "\tinc rsi\n"
         ^ (Printf.sprintf "\tjmp %s\n" label_loop_params)
         (* end of copy parameters *)
         ^ (Printf.sprintf "%s:\n" label_loop_params_end)
         (* allocate extenv[0] to point to new rib *)
         ^ "\tmov qword [rax], rbx\t; lambda simple : ext_env[0] <-- new_rib \n"
         ^ "\tmov rbx, rax\n"
         ^ "\tpop rax\n"
         (* allocate the closure object address in rax *)
         ^ "\tmov byte [rax], T_closure\n"
         (* set rax -> env = extenv *)
         ^ "\tmov SOB_CLOSURE_ENV(rax), rbx\n"
         (* set rax -> code = Lcode *)
         ^ (Printf.sprintf "\tmov SOB_CLOSURE_CODE(rax), %s\n" label_code)
         (* jump Lcont *)
         ^ (Printf.sprintf "\tjmp %s\n" label_end)
         (* Lcode: *)
         ^ (Printf.sprintf "%s:\t; lambda-simple body\n" label_code)
              (* make sure param list is ok *)
         ^ (Printf.sprintf "\tcmp qword [rsp + 8 * 2], %d\n"
              (List.length params'))
         ^ (Printf.sprintf "\tje %s\n" label_arity_ok)
         ^ "\tpush qword [rsp + 8 * 2]\n"
         ^ (Printf.sprintf "\tpush %d\n" (List.length params'))
         ^ "\tjmp L_error_incorrect_arity_simple\n"
         ^ (Printf.sprintf "%s:\n" label_arity_ok)
              (* push rbp *)
              (* mov rbp, rsp *)
         ^ "\tenter 0, 0\n"
              (* [[body]] *)
         ^ (run (List.length params') (env + 1) body)
              (* leave *)
         ^ "\tleave\n"
              (* ret *)
         ^ (Printf.sprintf "\tret 8 * (2 + %d)\n" (List.length params'))
         ^ (Printf.sprintf "%s:\t; lambda simple : new closure is in rax\n" label_end)
      | ScmLambda' (params', Opt opt, body) ->  (*TODO Mattan: FROM chapter 6 slides: page 100 *)
          let label_loop_env = make_lambda_opt_loop_env ()
          and label_loop_env_end = make_lambda_opt_loop_env_end ()
          and label_copy_params_loop = make_lambda_opt_loop_params ()
          and label_copy_params_loop_exit = make_lambda_opt_loop_params_end ()
          and label_params_loop = make_lambda_opt_loop_params ()
          and label_params_loop_exit = make_lambda_opt_loop_params_end ()
          and label_code = make_lambda_opt_code ()
          and label_arity_exact = make_lambda_opt_arity_exact ()
          and label_arity_more = make_lambda_opt_arity_more ()
          and label_stack_ok = make_lambda_opt_stack_ok ()
          and label_stack_expand_loop = make_lambda_opt_loop ()
          and label_stack_expand_loop_end = make_lambda_opt_loop_exit ()
          and label_stack_shrink_loop = make_lambda_opt_loop ()
          and label_stack_shrink_loop_exit = make_lambda_opt_loop_exit ()
          and label_end = make_lambda_opt_end ()
          and count = List.length params'
          in
          (*  allocate closure object *)
          "\tmov rdi, (1 + 8 + 8)\t; lambda opt : sob closure\n"
          ^ "\tcall malloc\n"
          ^ "\tpush rax\n"

          (* create ExtEnv *)
          (* create new rib for new env *)
          ^ (Printf.sprintf "\tmov rdi, 8 * %d\t; lambda opt : new rib\n" (List.length params'))
          ^ "\tcall malloc\n"
          ^ "\tpush rax\n"
          (* copy pointers *)
          ^ (Printf.sprintf "\tmov rdi, 8 * %d\t; lambda opt : extended env\n" (env + 1))
          ^ "\tcall malloc\n"
          ^ "\tmov rdi, ENV\n"
          ^ "\tmov rsi, 0\n"
          ^ "\tmov rdx, 1\n"
          ^ (Printf.sprintf "%s:\t; lambda opt : ext_env[i + 1] <-- env[i]\n"
                label_loop_env)
          ^ (Printf.sprintf "\tcmp rsi, %d\n" env)
          ^ (Printf.sprintf "\tje %s\n" label_loop_env_end)
          ^ "\tmov rcx, qword [rdi + 8 * rsi]\n"
          ^ "\tmov qword [rax + 8 * rdx], rcx\n"
          ^ "\tinc rsi\n"
          ^ "\tinc rdx\n"
          ^ (Printf.sprintf "\tjmp %s\n" label_loop_env)
          (* end of copy pointers *)
          ^ (Printf.sprintf "%s:\n" label_loop_env_end)
          ^ "\tpop rbx\n"
          ^ "\tmov rsi, 0\n"
          (* copy parameters off of the stack *)
          ^ (Printf.sprintf "%s:\t; lambda opt : copy params\n" label_copy_params_loop)
          ^ (Printf.sprintf "\tcmp rsi, %d\n" params)
          ^ (Printf.sprintf "\tje %s\n" label_copy_params_loop_exit)
          ^ "\tmov rdx, qword [rbp + 8 * rsi + 8 * 4]\n"
          ^ "\tmov qword [rbx + 8 * rsi], rdx\n"
          ^ "\tinc rsi\n"
          ^ (Printf.sprintf "\tjmp %s\n" label_copy_params_loop)
          (* end of copy parameters *)
          ^ (Printf.sprintf "%s:\n" label_copy_params_loop_exit)
          (* allocate extenv[0] to point to new rib *)
          ^ "\tmov qword [rax], rbx\t; lambda opt : ext_env[0] <-- new_rib \n"
          ^ "\tmov rbx, rax\n"
          ^ "\tpop rax\n"
          (* allocate the closure object address in rax *)
          ^ "\tmov byte [rax], T_closure\n"
          (* set rax -> env = extenv *)
          ^ "\tmov SOB_CLOSURE_ENV(rax), rbx\n"
          (* set rax -> code = Lcode *)
          ^ (Printf.sprintf "\tmov SOB_CLOSURE_CODE(rax), %s\n" label_code)
          (* jump Lcont *)
          ^ (Printf.sprintf "\tjmp %s\n" label_end)
          (* Lcode: *)
          ^ (Printf.sprintf "%s:\t; lambda-opt body\n" label_code)

          
            (* count param list length *)
          ^ (Printf.sprintf "\tcmp qword [rsp + 8 * 2], %d\n"
                (List.length params'))
            (* if as expected go to label exact *)
          ^ (Printf.sprintf "\tje %s\n" label_arity_exact)
          
          (* deal with more params *)
          ^ (Printf.sprintf "%s: \n" label_arity_more)
          ^ "\tmov rcx, qword[rsp + 8 * 2]\n" (*rcx holds n*)
          ^ "\tlea rsi, [rsp + 8 * (rcx + 2)]\n" (*rsi starts at the address of last parameter*)
          ^ "\tmov r8, rsi\n"
          ^ (Printf.sprintf "\tsub rcx, %d\n" count) (*amount of optional params*)
          ^ "\tmov r9, sob_nil\n"
          ^ (Printf.sprintf "%s: \t; params into heap loop\n" label_params_loop)
          ^ "\tcmp rcx, 0\n"
          ^ (Printf.sprintf "\tje %s\n" label_params_loop_exit)
          ^ "\tmov rdi, (1 + 8 +8)\n"
          ^ "\tcall malloc\n"
          ^ "\tmov byte[rax], T_pair\n"
          ^ "\tmov rbx, qword[rsi]\n"
          ^ "\tmov SOB_PAIR_CAR(rax), rbx\n"
          ^ "\tmov SOB_PAIR_CDR(rax), r9\n" (*link to next pair*)
          ^ "\tmov r9, rax\n"
          ^ "\tsub rsi, 8*1\n"
          ^ "\tdec rcx\n"
          ^ (Printf.sprintf "\tjmp %s\n" label_params_loop)
          ^ (Printf.sprintf "%s:\t; params into heap loop exit\n" label_params_loop_exit)
          (* r9 holds optional params list *)
          ^ "\tmov qword[r8], r9\n"
          ^ "\tadd rsi, 8*1\n"
          ^ (Printf.sprintf "\tmov rcx, %d\n" count)
          ^ "\tlea rsi, [rsp + 8 * (2 + rcx)]\n" (* pos of param at (count)*)
          ^ "\tsub r8, 8*1\n" (* pos of param n-2 *)
          ^ (Printf.sprintf "%s:\t; stack shrink\n" label_stack_shrink_loop)
          ^ "\tcmp rcx, 0\n"
          ^ (Printf.sprintf "\tje %s\n" label_stack_shrink_loop_exit)
          ^ "\tmov rax, qword[rsi]\n"
          ^ "\tmov qword[r8], rax\n"
          ^ "\tsub rsi, 8*1\n"
          ^ "\tsub r8, 8*1\n"
          ^ "\tdec rcx\n"
          ^ (Printf.sprintf "\tjmp %s\n" label_stack_shrink_loop)
          ^ (Printf.sprintf "%s:\t; stack shrink exit\n" label_stack_shrink_loop_exit)
          ^ (Printf.sprintf "\tmov qword[r8], %d\n" count)
          ^ "\tsub rsi, 8*1\n"
          ^ "\tsub r8, 8*1\n"
          ^ "\tmov rax, qword[rsi]\n"
          ^ "\tmov qword[r8], rax\n"
          ^ "\tsub rsi, 8*1\n"
          ^ "\tsub r8, 8*1\n"
          ^ "\tmov rax, qword[rsi]\n"
          ^ "\tmov qword[r8], rax\n"
          ^ "\tmov rsp, r8\n" (*fix rsp pos*)
          ^ (Printf.sprintf "\tjmp %s\n" label_stack_ok)

         (* deal with exact params *)
          ^ (Printf.sprintf "%s: \n" label_arity_exact)
          ^ "\tsub rsp, 8*1\n" (*one extra space on stack*)
          ^ "\tmov rdi, rsp\n"
          ^ "\tmov rax, qword[rdi + 8 * 1] ;move ret\n"
          ^ "\tmov qword[rdi], rax\n"
          ^ "\tadd rdi, 8*1\n"
          ^ "\tmov rax, qword[rdi + 8 * 1] ;move env\n"
          ^ "\tmov qword[rdi], rax\n"
          ^ "\tadd rdi, 8*1\n"
          ^ (Printf.sprintf "\tmov qword[rdi], %d\n" count)
          ^ (Printf.sprintf "\tlea r9, [rsp + 8 * (2 + %d)]\n" count)
          ^ (Printf.sprintf "%s : \n" label_stack_expand_loop)
          ^ "\tcmp rdi, r9\n"
          ^ (Printf.sprintf "\tje %s\n" label_stack_expand_loop_end)
          ^ "\tmov rax, qword [rdi + 8 * 1]\n"
          ^ "\tmov qword[rdi], rax\n"
          ^ "\tadd rdi, 8\n"
          ^ (Printf.sprintf "\tjmp %s \n" label_stack_expand_loop)
          ^ (Printf.sprintf "%s: \n" label_stack_expand_loop_end)
          ^ "\tmov qword[rdi], sob_nil\n"
          (* end deal with exact params *)
          ^ (Printf.sprintf "\tjmp %s \n" label_stack_ok)

          (* finish dealing with the stack *)
          ^ (Printf.sprintf "%s:\n" label_stack_ok)
          (* code section*)
          ^ "\tenter 0, 0\n"
          ^ (run count (env + 1) body)
          ^ "\tleave\n"
          (**)
          ^ (Printf.sprintf "\tret AND_KILL_FRAME(%d )\n" count)
          ^ (Printf.sprintf "%s:\t; lambda opt : new closure is in rax\n" label_end)


      | ScmApplic' (proc, args, Non_Tail_Call) -> (* DONE *)
        let reversed_args = List.rev args in
        let per_arg_exps = String.concat "" (List.map (fun arg -> (run params env arg) ^ "\tpush rax\n") reversed_args)
        in
        per_arg_exps ^ 
        Printf.sprintf "\tpush %d\n" (List.length args) ^
        (run params env proc) ^
        "\n\tassert_closure(rax)\n" ^
        "\tpush SOB_CLOSURE_ENV(rax)\n" ^
        "\tcall SOB_CLOSURE_CODE(rax)\n"

      | ScmApplic' (proc, args, Tail_Call) -> (*TODO Nadav: FROM chapter 6 slides: page 108 *)
        let argc = List.length args in
        let label_loop = make_tc_applic_recycle_frame_loop() in
        let label_done = make_tc_applic_recycle_frame_done() in
        let reversed_args = List.rev args in
        let per_arg_exps = String.concat "" (List.map (fun arg -> (run params env arg) ^ "\tpush rax\n") reversed_args) in
        let fix_stack = (Printf.sprintf "\tmov rsi, %d\n" (argc + 4)) ^
          "\tmov rcx, COUNT\n" ^
          "\tlea rcx, [rbp + 8*rcx + 8*3]\n" ^
          "\tlea rdx, [rbp - 8*1]\n" ^
          (Printf.sprintf "%s:\t; loop in scmapplic\n" label_loop) ^
          "\tcmp rsi, 0\n" ^
          (Printf.sprintf "\tje %s\n" label_done) ^
          "\tmov rdi, qword[rdx]\n" ^
          "\tmov qword[rcx], rdi\n" ^
          "\tsub rcx, 8\n" ^
          "\tsub rdx, 8\n" ^
          "\tdec rsi\n" ^
          (Printf.sprintf "\tjmp %s\n" label_loop) ^
          (Printf.sprintf "%s:\t; loop done in scmapplic\n" label_done) ^
          "\tadd rcx, 8\n" ^ 
          "\tmov rsp, rcx\n"
        in
        per_arg_exps ^ 
        (Printf.sprintf "\tpush %d\n" argc )^
        (run params env proc) ^
        "\n\tassert_closure(rax)\n" ^ 
        "\tpush qword [rbp + 8*1]\n" ^
        "\tpush qword [rbp]\n" ^
        fix_stack ^
        "\tpop rbp ; restore the old rbp\n" ^
        "\tjmp SOB_CLOSURE_CODE(rax)\n"
        
    and runs params env exprs' =
      List.map
        (fun expr' ->
          let code = run params env expr' in
          let code =
            code
            ^ "\n\tmov rdi, rax"
            ^ "\n\tcall print_sexpr_if_not_void\n" in
          code)
        exprs' in
    let codes = runs 0 0 exprs' in
    let code = String.concat "\n" codes in
    let code =
      (file_to_string "prologue-1.asm")
      ^ (asm_of_constants_table consts)
      ^ "\nsection .bss\n"
      ^ (asm_of_free_vars_table free_vars)
      ^ (file_to_string "prologue-2.asm")
      ^ (asm_of_global_bindings global_bindings_table free_vars)
      ^ "\n"
      ^ code
      ^ (file_to_string "epilogue.asm") in
    code;;

  let compile_scheme_string file_out user =
    let init = file_to_string "init.scm" in
    let source_code = init ^ user in
    let sexprs = (PC.star Reader.nt_sexpr source_code 0).found in
    let exprs = List.map Tag_Parser.tag_parse sexprs in
    let exprs' = List.map Semantic_Analysis.semantics exprs in
    let asm_code = code_gen exprs' in
    (string_to_file file_out asm_code;
     Printf.printf "!!! Compilation finished. Time to assemble!\n");;  


  let check_if_parses ocaml_content = 
    let sexprs = (PC.star Reader.nt_sexpr ocaml_content 0).found in
    let exprs = List.map Tag_Parser.tag_parse sexprs in
    let exprs' = List.map Semantic_Analysis.semantics exprs in
    true;;
  let compile_scheme_file file_in file_out =
    compile_scheme_string file_out (file_to_string file_in);;

end;; (* end of Code_Generation struct *)

(* end-of-input *)

