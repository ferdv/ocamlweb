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

(* This module is the heart of the program. The only function is
   [produce_document], which takes a list of files and produces the
   final \LaTeX\ document. *)


(*s Source file structure. *)

(* A source file is splitted into paragraphs of code and
  documentation. A new paragraph begins either when switching between
  code and comment or, within code, when an empty line occurs.

  A paragraph of documentation contains arbitrary text. A paragraph of
  CAML code is arbitrary CAML source text. A paragraph of LEX/YACC
  code is again a sequence of subparagraphs, which are either CAML
  source (actions), CAMLLEX syntax or CAMLYACC syntax *)


type sub_paragraph =
  | CamlCode of string
  | LexCode  of string
  | YaccCode of string
 
type paragraph =
  | Documentation of bool * int * string
  | RawLaTeX of string
  | Code of int * string
  | LexYaccCode of int * (sub_paragraph list)

(* A web section is a numbered part of a source file, which contains a
   sequence of paragraphs. The [sec_beg] field is the character
   position of the beginning of the web section inside the whole file *)
  
type raw_section = {
  sec_contents : paragraph list;
  sec_beg : int }

(* Finally, the contents of a source file is a sequence of web
   sections. The [content_file] field is the whole file name
   (including dirname and extension) whereas the [content_name] field
   is the corresponding module name *)

type content = { 
  content_file : string;
  content_name : string;
  content_contents : raw_section list } 

(*s A source file is either an implementation, an interface, a camllex
   description, a camlyacc description, or any other file, which is
   then considered as a \LaTeX\ file.  *)

type file = 
  | Implem of content
  | Interf of content
  | Lex    of content
  | Yacc   of content
  | Other  of string


(*s Options. 

    [index] indicates whether the index is to be produced; default value
    is [true].

    [extern_defs] indicates whether identifiers used but not defined should
    appear in the index; default value is [false].

    [web] indicates WEB style or not; default value is [true].

    [add_latex_option] passed an option to the \texttt{ocamlweb} \LaTeX\
    package. *)

val extern_defs : bool ref
val add_latex_option : string -> unit
val index : bool ref
val web : bool ref

(*s Main entry: production of the document from a list of files. *)

val produce_document : file list -> unit

