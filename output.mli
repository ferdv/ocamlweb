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

(*s In that module, we concentrate all the printing functions.
   Thus, it will be easy to add another kind of output, HTML for instance,
   by adapting the code of that module, and nothing else. 
   Default output is to standard output, but it can be redirected
   to a file with [set_output_to_file]. [close_output] closes the output,
   if it is a file. *)

val set_output_to_file : string -> unit
val close_output : unit -> unit

(*s These mutable flags controls the output. If [quiet], no extra
   output is done on standard output, default is [false]. If
   [use_greek_letters] is true, greek letters are used to display (some
   of) the single-letter type variables, default is [true]. *)

val quiet : bool ref
val use_greek_letters : bool ref
val short : bool ref

(*s Then we introduce some low level output functions, for characters 
   and strings.
   [(output_file f)] copies the contents of file [f] on the output. 
   [(output_verbatim s)] outputs the string [s] `as is'. *)

val output_char : char -> unit
val output_string : string -> unit
val output_file : string -> unit
val output_verbatim : string -> unit

(*s The following functions are mainly useful for a \LaTeX\ output,
    but will work correctly in other cases. A call to
    [set_no_preamble] suppresses the output of the header and
    trailer. [(end_line ())] ends a line, [(indentation n)] introduces
    an indentation of size [n] at the beggining of a line.
    [latex_header] takes as argument the options of the \LaTeX\
    package ocamlweb.sty. *)

val class_options : string ref
val set_no_preamble : bool -> unit
val push_in_preamble : string -> unit
val fullpage_headings : bool ref
val latex_header : string -> unit
val latex_trailer : unit -> unit

val indentation : int -> unit
val end_line : unit -> unit
val end_line_string : unit -> unit

val enter_math : unit -> unit
val leave_math : unit -> unit

(*s The following functions are used to pretty-print the code.
    [is_keyword] identifies the keywords of Objective Caml. 
    [output_ident] outputs an identifier, in different faces for keywords 
    and other identifiers, escaping the characters that need it, 
    like \_ for instance in \LaTeX. 
    [output_escaped_char] pretty-prints the reserved char of \LaTeX, 
    like \verb!&! or \verb!$!.
    [output_symbol] pretty-prints the Caml symbols,
    like $\rightarrow$ for \texttt{->}. 
    [output_type_variable s] pretty-prints type variables, in particular 
    one-letter type variables are output as greek letters. 
    [output_ascii_char n] outputs the character of ASCII code [n].
    *)

type char_type = Upper | Lower | Symbol
val what_is_first_char : string -> char_type

val is_keyword : string -> bool
val output_ident : string -> unit
val output_escaped_char : char -> unit
val output_symbol : string -> unit
val output_type_variable : string -> unit
val output_ascii_char : int -> unit

(* [output_lex_ident] (resp. [output_yacc_ident]) outputs an
   identifier as above but taking into account CAMLLEX keywords
   (resp. CAMLYACC keywords) *) 

val output_lex_ident : string -> unit
val output_yacc_ident : string -> unit

(*s Constants are typeset by the following functions. *)

val output_integer : string -> unit
val output_float : string -> unit

(*s Comments inside code are opened and closed respectively by
   [output_bc] and [output_ec]. The function [output_hfill] is called
   before [output_bc] to justify a comment. [output_byc] and
   [output_eyc] are the same for CAMLYACC comments, that is
   \verb|/*|\ldots\verb|*/|. *)

val output_bc : unit -> unit
val output_ec : unit -> unit
val output_byc : unit -> unit
val output_eyc : unit -> unit
val output_hfill : unit -> unit

(*s Strings inside code are opened and close respectively by
   [ output_bs] and [output_es]. A space character in a string is 
   output as a visible space, with [output_vspace]. *)

val output_bs : unit -> unit
val output_es : unit -> unit
val output_vspace : unit -> unit

(*s The following functions deal with sectioning. The highest level is
    the one of modules and interfaces. 
    The next level is the one of section. The last level is the one
    of paragraphs, which are atomic pieces of documentation or code. *)

val output_module : string -> unit
val output_interface : string -> unit
val output_lexmodule : string -> unit
val output_yaccmodule : string -> unit

val begin_section : unit -> unit

val begin_code : unit -> unit
val end_code : unit -> unit
val begin_dcode : unit -> unit
val end_dcode : unit -> unit

val begin_code_paragraph : unit -> unit
val end_code_paragraph : bool -> unit
val begin_doc_paragraph : bool -> int -> unit
val end_doc_paragraph : unit -> unit

(*s Index functions. [(output_index_entry id t def use)] outputs an entry line
    for identifier [id], with type [t], where [def] is the list of sections 
    where [f] is introduced and [use] the list of sections where [f] is used.
    If the type of the entry is [""], then it is omitted. *)

type 'a elem = Single of 'a | Interval of 'a * 'a

val begin_index : unit -> unit
val output_index_entry : 
  string -> string -> string elem list -> string elem list -> unit
val output_raw_index_entry : 
  string -> string -> string list -> string list -> unit
val end_index : unit -> unit

val output_label : string -> unit

(*s The parameters of the output engine are reset to their initial values
    with [reset_output]. *)

val reset_output : unit -> unit
