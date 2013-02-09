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

let items = new dictionary;;

(* Objects ********************************************************************)
class item dirName =
	let itemDir = envString#get "itemDir" in
	object (self)
		val dir = itemDir^dirName^"/"
		val data = 
			(new treeXml (itemDir^dirName^"/info.xml"))#getFirstByName "Item"
		val script = load_file (itemDir^dirName^"/script.lua")
		
		val mutable taken = false
		val mutable name = ""
		val mutable y = 0
		val mutable x = 0
		val mutable image = ""
		
		initializer
			name <- (data#getXmlElement ())#getAttr "name";
			x <- int_of_string(((data#getFirstByName "Position")#
				getXmlElement ())#getAttr "x");
			y <- int_of_string(((data#getFirstByName "Position")#
				getXmlElement ())#getAttr "y");
			image <- dir^((data#getFirstByName "Image")#getXmlElement ())#getAttr "src"
		
		method getDir =
			dir
		method getName =
			name
		method getImage =
			image
	end
;;

class room dirName =
	let roomDir = envString#get "roomDir" in
	object (self)
		val dir = roomDir^dirName^"/"
		val data = 
			(new treeXml (roomDir^dirName^"/info.xml"))#getFirstByName "Room"
		val script = load_file (roomDir^dirName^"/script.lua")
		
		val mutable name = ""
		val mutable background = ""
		val mutable itemsUsed = ([]:item list)
		
		initializer
			let itemsRead = 
				let listName = 
					cut ((((data#getFirstByName "Position")#getChildren ())#
					getXmlElement ())#getString ())
				in
				let rec getItems = function
					| [] -> []
					| e::q -> items#get e::getItems q
				in getItems listName
			in
			name <- (data#getXmlElement ())#getAttr "name";
			background <- dir^((data#getFirstByName "Image")#getXmlElement ())#getAttr "src";
			itemsUsed <- itemsRead
		
		method getDir =
			dir
		method getName =
			name
		method getBackground =
			background
	end
;;


class character dirName =
	let charDir = envString#get "characterDir" in
	object (self)
		val dir = charDir^dirName^"/"
		val personage = new treeXml (charDir^dirName^"/info.xml")
		val script = load_file (charDir^dirName^"/script.lua")
		
		val mutable name = ""
		val mutable y = 0 
		val mutable x = 0
		val mutable image = ""
		
		initializer
		name <- (personage#getXmlElement ())#getAttr "name";
		y <- int_of_string (((personage#getFirstByName "position")#getXmlElement ())#getAttr "y");
		x <- int_of_string (((personage#getFirstByName "position")#getXmlElement ())#getAttr "x");
		image <- ((personage#getFirstByName "image")#getXmlElement ())#getAttr "src";
		
		method getDir =
			dir
		method getName =
			name
		method getImage =
			image
		method getPos = 
			(x,y)
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

let loadItems () =
	let dirNameItems =
		let itemDir = envString#get "itemDir" in
		let elements = Array.to_list (Sys.readdir itemDir) in
		let rec sort = function
			| [] -> []
			| e::q when Sys.is_directory ((Filename.concat itemDir) e) -> 
				e::sort q
			| _::q -> sort q
		in sort elements
	in 
	let rec browser = function 
		| [] -> ()
		| e::q -> items#set e (new item e); browser q
	in browser dirNameItems
;;
