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
let envString = new dictionary;;

let globalInt = new dictionary;;
let globalString = new dictionary;;

(* Objects ********************************************************************)
class item name =
	let itemDir = envString#get "itemDir" in
	object (self)
		val dir = itemDir^name^"/"
		val data = 
			(new treeXml (itemDir^name^"/info.xml"))#getFirstByName "Item"
		val script = load_file (itemDir^name^"/script.lua")
		
		val mutable name = ""
		val mutable y = 0
		val mutable x = 0
		val mutable image = ""
		
		initializer
			name <- (data#getXmlElement ())#getAttr "name";
			x <- int_of_string(((data#getFirstByName "Position")#getXmlElement ())#getAttr "x");
			y <- int_of_string(((data#getFirstByName "Position")#getXmlElement ())#getAttr "y");
			image <- dir^((data#getFirstByName "Image")#getXmlElement ())#getAttr "src"
		
		method getDir =
			dir
		method getName =
			name
		method getImage =
			image
	end
;;

class room name =
	let roomDir = envString#get "roomDir" in
	object (self)
		val dir = roomDir^name^"/"
		val data = 
			(new treeXml (roomDir^name^"/info.xml"))#getFirstByName "Room"
		val script = load_file (roomDir^name^"/script.lua")
		
		val mutable name = ""
		val mutable background = ""
		
		initializer
			name <- (data#getXmlElement ())#getAttr "name";
			background <- dir^((data#getFirstByName "Image")#getXmlElement ())#getAttr "src"
		
		method getDir =
			dir
		method getName =
			name
		method getBackground =
			background
	end
;;

(* Functions ******************************************************************)
let getEnvString name =
	(envString#get name:string)
;;

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


