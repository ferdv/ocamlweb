(*
 * ocamlweb - A WEB-like tool for ocaml
 * Copyright (C) 1999-2001 Jean-Christophe FILLIÂTRE and Claude MARCHÉ
 * 
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file LGPL).
 *)

(*i $Id$ i*)

{
  open Printf
  open Output
  open Lexing

(*s Global variables and functions used by the lexer. *)

(* [comment_depth] indicates how many opening-braces we saw, so we know how
   many closing-braces we have to look for, in order to respect 
   caml's-specifications concerning comments imbrication. *)

  let comment_depth = ref 0

(* Accounts for braket-depth in order to imbricate them. *)

  let bracket_depth = ref 0

(* Set a reference on the starting character when we see [\verb]. *)

  let verb_delim = ref (Char.chr 0)

(* counts occurences of "%%" in yacc files *)

  let yaccdoublepercentcounter = ref 0

(* This function returns the first char of a lexbuf. *)

  let first_char lexbuf = lexeme_char lexbuf 0

(* The [count_spaces] function acccounts for spaces in order to respect
   alignment (in the \LaTeX{}-outputed file) concerning left margins. *)

  let count_spaces s =
    let c = ref 0 in
    for i = 0 to String.length s - 1 do
      if s.[i] = '\t' then
	c := !c + (8 - (!c mod 8))
      else
	incr c
    done;
    !c

(* This boolean value is true if we enter in math mode, false otherwise. *)

  let user_math_mode = ref false

(* [user_math] function does everything is needed to set the math mode
   when it is called, particularly it checks/sets the [user_math_mode]
   value as needed. *)

  let user_math () =
    if not !user_math_mode then begin
      user_math_mode := true;
      enter_math ()
    end else begin
      user_math_mode := false;
      leave_math ()
    end

(* Checks if we are in maths mode and prints char c considering the case. *)

  let check_user_math c =
    if !user_math_mode then output_char c else output_escaped_char c

(* This function sets values in order to reset the lexer, so we could call it 
   on an another file. *)

  let reset_pretty () =
    reset_output ();
    yaccdoublepercentcounter := 0;
    user_math_mode := false

}


(*s Shortcuts for regular expressions. *)

let space = [' ' '\t']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
(* This is for the identifiers as specified in caml's specifications. *)
let identchar = 
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let identifier = (lowercase | uppercase) identchar*
(* This one helps protecting special caracters. *)
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let caml_token =
    "[" | "]" | "[|" | "|]" | "[<" | ">]" | "{" | "}" | "{<" | ">}" | "[]" 
  | "(" | ")" | "or" | "not" | "||"
let symbol_token =
  caml_token | (symbolchar+)
let character = 
  "'" ( [^ '\\' '\''] | '\\' ['\\' '\'' 'n' 't' 'b' 'r'] 
      | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] ) "'"
let decimal_literal = ['0'-'9']+
let hex_literal = '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+
let oct_literal = '0' ['o' 'O'] ['0'-'7']+
let bin_literal = '0' ['b' 'B'] ['0'-'1']+
let float_literal =
  ['0'-'9']+ ('.' ['0'-'9']*)? (['e' 'E'] ['+' '-']? ['0'-'9']+)?

(*s Pretty-printing of code. Main entry points for Caml and Lex and
    Yacc files, counts for spaces in order to respect alignment.  The
    following function pretty-prints some code and assumes that we are
    at the beginning of a line. *)

rule pr_camlcode = parse
  | space* { let n = count_spaces (lexeme lexbuf) in indentation n;
	     pr_camlcode_inside lexbuf; pr_camlcode lexbuf }
  | eof    { leave_math () }

and pr_lexcode = parse
  | space* { let n = count_spaces (lexeme lexbuf) in indentation n;
	     pr_lexcode_inside lexbuf; pr_lexcode lexbuf }
  | eof    { leave_math () }

and pr_yacccode = parse
  | space* { let n = count_spaces (lexeme lexbuf) in indentation n;
	     pr_yacccode_inside lexbuf; pr_yacccode lexbuf }
  | eof    { leave_math () }

  
(*s That function pretty-prints the Caml code anywhere else. *)

and pr_camlcode_inside = parse
  | '\n' 
      { end_line () }
  | space+
      { output_char '~'; pr_camlcode_inside lexbuf }
  | character
      { output_verbatim (lexeme lexbuf); pr_camlcode_inside lexbuf }
  | "'" identifier
      { let id = lexeme lexbuf in
	output_type_variable (String.sub id 1 (pred (String.length id))); 
	pr_camlcode_inside lexbuf }
  | "(*r" 
      { output_hfill (); output_bc (); comment_depth := 1;
	pr_comment lexbuf; pr_camlcode_inside lexbuf }
  | "(*"
      { output_bc (); comment_depth := 1;
	pr_comment lexbuf; pr_camlcode_inside lexbuf }
  | '"'  
      { output_bs (); pr_code_string lexbuf; pr_camlcode_inside lexbuf }
  | symbol_token
      { output_symbol (lexeme lexbuf); pr_camlcode_inside lexbuf }
  | (identifier '.')* identifier
      { output_ident (lexeme lexbuf); pr_camlcode_inside lexbuf }
  | eof
      { () (*i end_line() supprime par dorland-muller i*) }
  | decimal_literal | hex_literal | oct_literal | bin_literal
      { output_integer (lexeme lexbuf); pr_camlcode_inside lexbuf }
  | float_literal
      { output_float (lexeme lexbuf); pr_camlcode_inside lexbuf }
  | _ 
      { output_escaped_char (first_char lexbuf); pr_camlcode_inside lexbuf }


(*s That function pretty-prints the Lex code anywhere else. *)

and pr_lexcode_inside = parse 
  | '_'  
      { output_string "\\ocwlexwc"; pr_lexcode_inside lexbuf } 
  | '*'  
      { enter_math (); 
        output_string "^\\star{}";
        pr_lexcode_inside lexbuf } 
  | '+'  
      { enter_math (); 
        output_string "^{\\scriptscriptstyle +}";
        pr_lexcode_inside lexbuf } 
  | '-'  
      { leave_math ();
	output_string "--";
        pr_lexcode_inside lexbuf }
  | '|'
      { enter_math (); output_string "\\mid{}"; pr_lexcode_inside lexbuf }
  | '\n' 
      { end_line () }
  | space+
      { output_char '~'; pr_lexcode_inside lexbuf }
  | character
      { output_verbatim (lexeme lexbuf); pr_lexcode_inside lexbuf }
  | "(*" { output_bc (); comment_depth := 1;
	   pr_comment lexbuf; pr_lexcode_inside lexbuf }
  | "(*r" 
      { output_hfill (); output_bc (); comment_depth := 1;
	pr_comment lexbuf; pr_lexcode_inside lexbuf }
  | '"'  { output_bs (); pr_code_string lexbuf; pr_lexcode_inside lexbuf }
  | identifier
      { output_lex_ident (lexeme lexbuf); pr_lexcode_inside lexbuf }
  | eof  { () }
  | _ 
      { output_escaped_char (first_char lexbuf); pr_lexcode_inside lexbuf }
      
(*s That function pretty-prints the Yacc code anywhere else. *)
and pr_yacccode_inside = parse
  | "%%" 
      {
	incr yaccdoublepercentcounter;
	output_string 
	  (if !yaccdoublepercentcounter = 1 
	   then "\\ocwyaccrules"
	   else "\\ocwyacctrailer");
	pr_yacccode_inside lexbuf
      }
  | "%{"
      {
	output_string "\\ocwyaccopercentbrace";
	pr_yacccode_inside lexbuf
      }
  | "%}"
      {
	output_string "\\ocwyacccpercentbrace";
	pr_yacccode_inside lexbuf
      }
  | ":"
      {
	output_string "\\ocwyacccolon";
	pr_yacccode_inside lexbuf
      }
  | ";"
      {
	output_string "\\ocwyaccendrule";
	pr_yacccode_inside lexbuf
      }
  | "|" 
      {
	output_string "\\ocwyaccpipe";
	pr_yacccode_inside lexbuf
      }
  | '\n' 
      { end_line (); }
  | space+
      { output_char '~'; pr_yacccode_inside lexbuf }
  | "/*r"
      { output_hfill (); output_byc (); pr_yacc_comment lexbuf; 
	pr_yacccode_inside lexbuf }
  | "/*" 
      { output_byc (); pr_yacc_comment lexbuf; pr_yacccode_inside lexbuf }
  | '%'? identifier
      { output_yacc_ident (lexeme lexbuf); pr_yacccode_inside lexbuf }
  | _ 
      { output_escaped_char (first_char lexbuf); pr_yacccode_inside lexbuf }
  | eof 
      { () }


(*s Comments. *)

and pr_comment = parse
  | "(*" 
      { output_bc (); 
	incr comment_depth;
	pr_comment lexbuf }
  | "*)" 
      { output_ec (); 
	decr comment_depth;
	if !comment_depth > 0 then pr_comment lexbuf }
(*i utile ????? 
  | '\n' space* '*' ' '
         { output_string "\n "; pr_comment lexbuf } i*)
  | '"' 
      { output_bs (); 
	pr_code_string lexbuf; 
	pr_comment lexbuf; }
  | '['  
      { if !user_math_mode then 
	  output_char '['
	else begin
	  bracket_depth := 1; 
	  begin_dcode (); escaped_code lexbuf; end_dcode ()
	end; 
	pr_comment lexbuf }
  | eof  
      { () }
  | "\\$" 
      { output_string (lexeme lexbuf); pr_comment lexbuf }
  | '$'  
      { user_math(); pr_comment lexbuf }
  | _    
      { output_char (first_char lexbuf); pr_comment lexbuf }


(* The [C_like_comments] are not inbricable *)
and pr_yacc_comment = parse
  | "*/" { output_eyc (); } 
  | '\n' space* '*' ' '
         { output_string "\n "; pr_yacc_comment lexbuf }
  | '['  { if !user_math_mode then 
	     output_char '['
	   else begin
	     bracket_depth := 1; 
	     begin_dcode (); escaped_code lexbuf; end_dcode ()
	   end; 
	   pr_yacc_comment lexbuf }
  | eof  { () }
  | "\\$" { output_string (lexeme lexbuf); pr_yacc_comment lexbuf }
  | '$'  { user_math(); pr_yacc_comment lexbuf }
  | _    { output_char (first_char lexbuf); pr_yacc_comment lexbuf }


(*s Strings in code. *)

and pr_code_string = parse
  | '"'  { output_es () }
  | '\n' { end_line_string (); pr_code_string lexbuf }
  | ' '  { output_vspace (); pr_code_string lexbuf }
  | '\\' ['"' 't' 'b' 'r']
         { output_escaped_char '\\'; 
	   output_char (lexeme_char lexbuf 1); 
	   pr_code_string lexbuf }
  | '\\' '\n'
         { output_escaped_char '\\'; end_line_string ();
	   pr_code_string lexbuf }
  | '\\' '\\'
         { output_escaped_char '\\'; output_escaped_char '\\'; 
	   pr_code_string lexbuf }
  | eof  { () }
  | '-'  { output_ascii_char 45; pr_code_string lexbuf }
  | _    { output_escaped_char (first_char lexbuf); pr_code_string lexbuf }


(*s Escaped code. *)

and escaped_code = parse
  | '['  { output_char '['; incr bracket_depth; escaped_code lexbuf }
  | ']'  { decr bracket_depth; 
	   if !bracket_depth > 0 then begin
	     output_char ']'; escaped_code lexbuf
           end else
	     if not !user_math_mode then leave_math () }
  | '"'  { output_bs (); pr_code_string lexbuf; escaped_code lexbuf }
  | space+
         { output_char '~'; escaped_code lexbuf }
  | character
         { output_verbatim (lexeme lexbuf); escaped_code lexbuf }
  | "'" identifier
         { let id = lexeme lexbuf in
	   output_type_variable (String.sub id 1 (pred (String.length id))); 
	   escaped_code lexbuf }
  | symbol_token
         { output_symbol (lexeme lexbuf); escaped_code lexbuf }
  | identifier
         { output_ident (lexeme lexbuf); escaped_code lexbuf }
  | eof  { if not !user_math_mode then leave_math () }
  | decimal_literal | hex_literal | oct_literal | bin_literal
         { output_integer (lexeme lexbuf); escaped_code lexbuf }
  | float_literal
         { output_float (lexeme lexbuf); escaped_code lexbuf }
  | _    { output_escaped_char (first_char lexbuf); escaped_code lexbuf }


(*s Documentation. 
    It is output `as is', except for quotations. *)

and pr_doc = parse
  | '[' 
      { if !user_math_mode then 
	  output_char '['
	else begin
	  bracket_depth := 1; 
	  begin_dcode (); escaped_code lexbuf; end_dcode ()
	end; 
	pr_doc lexbuf }
  | "\\$" 
      { output_string (lexeme lexbuf); pr_doc lexbuf }
  | '$' 
      { user_math(); pr_doc lexbuf }
  | "\\verb" _  
      { verb_delim := lexeme_char lexbuf 5;
        output_string (lexeme lexbuf);
	pr_verb lexbuf; pr_doc lexbuf }
  | "\\begin{verbatim}"
      { output_string (lexeme lexbuf);
	pr_verbatim lexbuf; pr_doc lexbuf }
  | eof 
      { () }
  | _   
      { output_char (first_char lexbuf); pr_doc lexbuf }

and pr_doc_title = parse
  | '[' 
      { if !user_math_mode then 
	  output_char '['
	else begin
	  bracket_depth := 1; 
	  begin_dcode (); escaped_code lexbuf; end_dcode ()
	end; 
	pr_doc_title lexbuf }
  | '.'
      { output_string ".}\\quad{}" }
  | eof
      { () }
  | _ 
      { output_char (first_char lexbuf); pr_doc_title lexbuf }

and pr_verb = parse
  | eof  { () }
  | _    { let c = lexeme_char lexbuf 0 in
	   output_char c;
           if c == !verb_delim then () else pr_verb lexbuf }

and pr_verbatim = parse
  | "\\end{verbatim}"
         { output_string (lexeme lexbuf) }
  | eof  { () }
  | _    { output_char (lexeme_char lexbuf 0); pr_verbatim lexbuf }

{

(*s pretty-printing subparagraphs *)

  let pretty_print_caml_subpar s = 
    pr_camlcode (Lexing.from_string s)

  let pretty_print_lex_subpar s = 
    pr_lexcode (Lexing.from_string s)

  let pretty_print_yacc_subpar s = 
    pr_yacccode (Lexing.from_string s)

(*s Then we can introduce two functions [pretty_print_code] and 
    [pretty_print_doc], which pretty-print respectively code and
    documentation parts. 
 *)

  let pretty_print_code is_last_paragraph s = 
    pr_camlcode (Lexing.from_string s);
    end_code_paragraph is_last_paragraph

  let pretty_print_doc is_first_paragraph (big,n,s) = 
    begin_doc_paragraph is_first_paragraph n;
    let lb = Lexing.from_string s in
    if big then begin output_string "\\textbf{"; pr_doc_title lb end;
    pr_doc lb;
    end_doc_paragraph ()

}
