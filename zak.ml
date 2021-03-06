(* Zak - Scene manager
################################################################################
#    Wins is a "Point and Click" Game Motor written with OCaml                 #
#    Copyright (C) 2013    Philémon Gardet [philemon.gardet@epita.fr]          #
#                          Rafael Gozlan [rafael.gozlan@epita.fr]              #
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
let rooms = new dictionary;;

let inventory = ref [];;

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
		
		val mutable name = ""
		val mutable thumbnail = None
		
		initializer
			name <- (data#getXmlElement ())#getAttr "name";
			thumbnail <- Some ((data#getXmlElement ())#getAttr "thumbnail");
			print_string ("├ Item "^dirName^" loaded.\n")
		
		method getDir =
			dir
		method getName =
			name
		method getDataAnim =
			animation
		method getThumnail = 
			let getter = function
			| Some img -> img
			| None -> failwith "get thumnail of item "^name^" (but it impossible)."
		in getter thumbnail
			
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
		
		val mutable name = ""
		val mutable font = ""
		val mutable color = ""
		
		initializer
			let fontData = data#getFirstByName "font"
			in
			name <- data#getAttr "name";
			font <- fontData#getAttr "name";
			color <-fontData#getAttr "color"; 
			print_string ("├ Character "^dirName^" loaded.\n")
		
		method getDir =
			dir
		method getName =
			name
		method getDataAnim =
			animation
		method getFont =
			(font,color)
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
		val dialog = 
			(new treeXml (roomDir//dirName//"dialog.xml"))#getFirstByName "Dialogs"
		val nodes = 
			let graphXml = 
				(new treeXml (roomDir//dirName//"nodes.xml"))#getFirstByName "graph"
			in new graph graphXml
		val script = roomDir//dirName//"script.lua"
		
		val mutable name = ""
		val mutable background = ""
		
		initializer
			name <- (data#getXmlElement ())#getAttr "name";
			background <- dir//((data#getFirstByName "Background")#getXmlElement ())#getAttr "src";
			print_string ("├ Room "^dirName^" loaded.\n")
		
		method getDir =
			dir
		method getName =
			name
		method getBackground =
			background
		method getDialog =
			dialog
		method getNodes = 
			nodes
		method getScript =
			script
	end
;;

(* Functions ******************************************************************)
(** Accessor environement variables **)
let getEnvString name =
	(envString#get name:string)
;;

(** Accessor and modifier global game variables **)
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

(** Setup items, characters and rooms objects **)
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

let loadRooms () =
	let dirNameRooms =
		let roomDir = envString#get "roomDir" in
		let elements = Array.to_list (Sys.readdir roomDir) in
		let rec sort = function
			| [] -> []
			| e::q when Sys.is_directory ((Filename.concat roomDir) e) -> 
				e::sort q
			| _::q -> sort q
		in sort elements
	in 
	let rec browser = function 
		| [] -> ()
		| e::q -> rooms#set e (new room e); browser q
	in browser dirNameRooms
;;

(** Accessor items and characters objects **)
let getItem name =
	items#get name
;;

let getCharacter name =
	characters#get name
;;

let getRoom name =
	rooms#get name

(** Manage player's inventory **)
let invAddItem (name:string) =
  let rec browser = function
    | [] -> name::[]
    | e::q when name > e -> name::e::q
    | e::q -> e::browser q
  in inventory := browser !inventory
;;

let invGetItems () =
	!inventory

let invCheckItem name =
  List.mem name !inventory
;;

let invIsNotVoid () =
	!inventory <> []

let invDropItem name =
  let rec browser = function
    | [] -> []
    | e::q when name > e -> e::q
    | e::q when name = e -> q
    | e::q -> e::browser q
  in inventory := browser !inventory
;;
