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
let characters = new dictionary;;

(* Interface ******************************************************************)
class type displayable =
	object
		method getDir : string
		method getName : string
		method getDataAnim : treeXml
	end
;;

(* Objects ********************************************************************)
class item dirName =
	let itemDir = envString#get "itemDir" in
	object (self)
		val dir = itemDir//dirName
		val data =
			try (new treeXml (itemDir//dirName//"info.xml"))#getFirstByName "Item"
			with _ -> failwith ("read info.xml of item "^dirName^" failed.")
		val animation = 
			try new treeXml (itemDir//dirName//"animation.xml")
			with _ -> failwith ("read animation.xml of item "^dirName^" failed.")
		val script = 
			try load_file (itemDir//dirName//"script.lua")
			with _ -> failwith ("read script.lua of item "^dirName^" failed.")
		
		val mutable taken = false
		val mutable name = ""
		
		initializer
			name <- (data#getXmlElement ())#getAttr "name";
			print_string ("├ Item "^dirName^" loaded.\n")
		
		method getDir =
			dir
		method getName =
			name
		method getDataAnim =
			animation
	end
;;

class character dirName =
	let charDir = envString#get "characterDir" in
	object (self)
		val dir = charDir//dirName
		val data = 
			try (new treeXml (charDir//dirName//"info.xml"))#getFirstByName "Character"
			with _ -> failwith ("read info.xml of character "^dirName^" failed.")
		val animation = 
			try new treeXml (charDir//dirName//"animation.xml")
			with _ -> failwith ("read animation.xml of character "^dirName^" failed.")
		val script = 
			try load_file (charDir//dirName//"script.lua")
			with _ -> failwith ("read script.lua of character "^dirName^" failed.")
		
		val mutable name = ""
		
		initializer
			name <- data#getAttr "name";
			print_string ("├ Character "^dirName^" loaded.\n")
		
		method getDir =
			dir
		method getName =
			name
		method getDataAnim =
			animation
		method sayHello =
			"hello"
	end
;;

class room dirName =
	let roomDir = envString#get "roomDir" in
	object (self)
		val dir = roomDir//dirName
		val data = 
			(new treeXml (roomDir//dirName//"info.xml"))#getFirstByName "Room"
		val script = load_file (roomDir//dirName//"script.lua")
		
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

(* Functions ******************************************************************)
(* Accessor environement variables *)
let getEnvString name =
	(envString#get name:string)
;;

(* Accessor and modifier global game variables *)
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

(* Setup items and characters objects *)
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

let loadCharacters () =
	let dirNameCharacters =
		let charDir = envString#get "characterDir" in
		let elements = Array.to_list (Sys.readdir charDir) in
		let rec sort = function
			| [] -> []
			| e::q when Sys.is_directory ((Filename.concat charDir) e) -> 
				e::sort q
			| _::q -> sort q
		in sort elements
	in 
	let rec browser = function 
		| [] -> ()
		| e::q -> characters#set e (new character e); browser q
	in browser dirNameCharacters
;;

(* Accessor items and characters objects *)
let getItem name =
	items#get name
;;

let getCharacter name =
	characters#get name
;;
