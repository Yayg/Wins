(* Zak - Scene manager
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

open Tool
open Wally

(* Global Variables ***********************************************************)
let globalInt = new dictionary;;
let globalString = new dictionary;;

(* Objects ********************************************************************)
class item name =
	let itemDir = globalString#get "itemDir" in
	object (self)
		val data = 
			(new treeXml (load_file (itemDir^name^"\\info.xml")))#getFirstByName "Item"
		val script = load_file (itemDir^name^"script.lua")
		
		val mutable name = ""
		val mutable image = ""
		
		initializer
			name <- (data#getXmlElement ())#getAttr "name";
			image <- ((data#getFirstByName "Image")#getXmlElement ())#getAttr "src"
		
		method getName =
		 name
		method getImage =
			image
	end
;;

(* Functions ******************************************************************)
let setGlobalInt name (value:int) =
	globalInt#set name value
;;

let setGlobalString name (text:string) =
	globalString#set name text

let getGlobalInt name =
	globalInt#get name
;;

let getGlobalString name =
	globalString#get name
;;

let removeGlobalInt name =
	globalInt#remove name
;;

let removeGlobalString name =
	globalString#remove name
;;


