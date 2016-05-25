(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let fast = ref false                    (* -unsafe *)
and applicative_functors = ref true     (* -no-app-funct *)

let parse_color_setting = function
  | "auto" -> Some Misc.Color.Auto
  | "always" -> Some Misc.Color.Always
  | "never" -> Some Misc.Color.Never
  | _ -> None
let color = ref Misc.Color.Auto ;; (* -color *)
