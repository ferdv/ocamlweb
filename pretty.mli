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

(*s The following functions pretty-print the paragraphs of code and
    documentation, respectively. The boolean argument indicates
    whether the given paragraph is the last one for
    [pretty_print_code] or the first one for [pretty_print_doc]. *)

val pretty_print_code : bool -> string -> unit
val pretty_print_doc  : bool -> bool * int * string -> unit

(*s These three functions pretty-print subparagraphs of Caml code,
Camllex code and Camlyacc code respectively *)

val pretty_print_caml_subpar : string -> unit
val pretty_print_lex_subpar : string -> unit
val pretty_print_yacc_subpar : string -> unit

(*s This function sets values in order to reset the lexer, so we could
   call it on an another file. *)

val reset_pretty : unit -> unit

val count_spaces : string -> int
