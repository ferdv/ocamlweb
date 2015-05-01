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

(* $Id: yacc_syntax.mli,v 1.6 2002-02-01 09:40:05 marche Exp $ *)

(* locations for refering to CAML parts of yacc files *)

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

(*

  current file being parsed, current lexer buffer,

*)

val current_file_name : string ref
val current_line_num : int ref
val current_line_start_pos : int ref
val current_lexbuf : Lexing.lexbuf ref
val issue_warning : string -> unit



