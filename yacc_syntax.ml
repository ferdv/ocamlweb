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

(* $Id$ *)

(*s locations for refering to CAML parts of yacc files *)

type location = Lex_syntax.location

type ident = string * location

type token_decls =
  | Typed_tokens of location * ident list      (*r \verb|%token <type> ...| *)
  | Untyped_tokens of ident list               (*r \verb|%token ...| *)
  | Non_terminals_type of location * ident list (*r \verb|%type <type> ...| *)
  | Start_symbols of ident list          (*r \verb|%start ...| *)
  | Tokens_assoc of ident list        (*r \verb|%left|, \verb|%right| or \verb|%nonassoc| *)


type yacc_definitions =
    {
      header : location ;
      decls : token_decls list;
      rules : (ident * (ident list * location) list) list ;
      trailer : location
    }


let current_file_name = ref ""
let current_line_num = ref 0
let current_line_start_pos = ref 0
let current_lexbuf = ref (Lexing.from_string "")

let issue_warning msg =
  if not !Output.quiet then
    let line = !current_line_num
    and column = 
      Lexing.lexeme_start !current_lexbuf - !current_line_start_pos 
    in 
    Printf.eprintf "Warning in file %s at line %d, character %d: %s\n" 
      !current_file_name line column msg
      
