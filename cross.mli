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

(*s That module exports to global tables [used] and [defined], indexed
   by identifiers (strings) and containing respectively the sets of locations
   where they are defined and used.
   Those locations are of type [where], which contain the name of the file
   and the absolute position in the source.
 *)
   
type where = { w_filename : string; w_loc : int }

type entry_type = 
  | Value
  | Constructor
  | Field
  | Label
  | Type
  | Exception
  | Module
  | ModuleType
  | Class
  | Method
  | LexParseRule       (*r CAMLLEX entry points *)
  | RegExpr            (*r CAMLLEX regular expressions *)
  | YaccNonTerminal    (*r CAMLYACC non-terminal symbols *)
  | YaccTerminal       (*r CAMLYACC terminal symbols, i.e. tokens *)
		    
type index_entry = { e_name : string; e_type : entry_type }

module Idmap : Map.S with type key = index_entry

module Stringset : Set.S with type elt = string

module Whereset : Set.S with type elt = where

val used : Whereset.t Idmap.t ref
val defined : Whereset.t Idmap.t ref

(*s The two following functions fill the above tables for a given file. *)

val cross_implem : string -> string -> unit
val cross_interf : string -> string -> unit
 

(* cross-referencing lex and yacc description files *)

val cross_lex : string -> string -> unit
val cross_yacc : string -> string -> unit
 
