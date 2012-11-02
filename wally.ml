(* Wally - The Lua Motor
################################################################################
#    Wins is a "Point and Click" Game Motor written with OCaml                 #
#    Copyright (C) 2013    Philémon Gardet [philemon.gardet@epita.fr]          #
#                          Rafaël Gozlan [rafael.gozlan@epita.fr]              #
#                          Lorry Guedj [lorry.guedj@epita.fr]                  #
#                          Alexandre Starck [alexandre.starck@epita.fr]        #
#                                                                              #
#    This program is free software: you can redistribute it and/or modify      #
#    it under the terms of the GNU General Public License as published by      #
#    the Free Software Foundation, either version 3 of the License, or         #
#    (at your option) any later version.                                       #
#                                                                              #
#    This program is distributed in the hope that it will be useful,           #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of            #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             #
#    GNU General Public License for more details.                              #
#                                                                              #
#    You should have received a copy of the GNU General Public License         #
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.     #
################################################################################
*)

open Lua_api
open Tool


(* Global Variables ***********************************************************)
(** Lua runtime environment. *)
let luaEnv = LuaL.newstate ();;
(** Global Counters dictionary *)
let globalCounts = new dictionary;;
(* Functions ******************************************************************)

let initWally () =
	LuaL.openlibs luaEnv
;;

let registerFunction name func = 
	Lua.register luaEnv name !func
;;

let registerGlobalCounts name (value:int) =
	globalCounts#put name value
;;

let updateGlobalCounts name value =
	globalCounts#update name value
;;

let getGlobalCounts name =
	globalCounts#get name
;;

let removeGlobalCounts name =
	globalCounts#remove name
;;
