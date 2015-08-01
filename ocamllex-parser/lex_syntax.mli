(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* The shallow abstract syntax *)

type location =
    { start_pos: Lexing.position;
      end_pos: Lexing.position;
      start_line: int;
      start_col: int }

type regular_expression =
    Epsilon
  | Characters of int list
  | Sequence of regular_expression * regular_expression
  | Alternative of regular_expression * regular_expression
  | Repetition of regular_expression
  | Ident of string * location

type lexer_definition =
    { header: location;
      named_regexps : (string * location * regular_expression) list;
      entrypoints: (string * location * (regular_expression * location) list) list;
      trailer: location }
