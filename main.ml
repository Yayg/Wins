(* Main - The Main File
################################################################################
#                                                                              #
#     oooooo   oooooo     oooo  o8o           A project by students EPITA      #
#      `888.    `888.     .8'   `"'                          in infosup 2017   #
#       `888.   .8888.   .8'   oooo  ooo. .oo.    .oooo.o                      #
#        `888  .8'`888. .8'    `888  `888P"Y88b  d88(  "8                      #
#  Wins   `888.8'  `888.8'      888   888   888  `"Y88b.                       #
#  is not  `888'    `888'       888   888   888  o.  )88b                      #
#  Scumm !  `8'      `8'       o888o o888o o888o 8""888P'                      # 
#                                                              Programmed by   #
#    Philémon "Phil Gekni" Gardet [philemon.gardet@epita.fr]                   #
#    Rafael "Yayg" Gozlan [rafael.gozlan@epita.fr]                             #
#                                                              with help       #
#    Lorry "Bardaf" Guedj [lorry.guedj@epita.fr]                               #
#    Alexandre "Stawayld" Starck [alexandre.starck@epita.fr]                   #
#                                                                              #
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

open Wally
open Zak
open Tool
open Lua_api

let usage_msg = "Usage : wins [gameFolder]\n";;

(* Main function **************************************************************)
let main () = 
	let _ =
		let w =
			Jed.initW ();
			Jed.getWindow ()
		in
		w#changeRoom (envString#get "firstRoom") (envString#get "firstNode") ()
	in
	Woop.bigLoop();
	exit 0
;;

(* Registration function Lua **************************************************)
let registerStaticFuncLua () =
	(** print_debug (string) : print a debug message in the console **)
	let printdebug state =
		match (Lua.tostring state 1) with
			| Some s -> print_debug ("Script : "^s)
			| _ -> print_debug "script <"
	
	and getenvironementvar state =
		let value = match (Lua.tostring state 1) with
			| Some name -> getEnvString name
			| _ -> "Invalid call in lua at get_environement_var"
		in Lua.pushstring state value
	
	and setglobalint state =
		match (Lua.tostring state 1),(Lua.tointeger state 2) with
			| Some name,value -> setGlobalInt name value
			| _ -> failwith "Invalid call in lua at set_global_int"
	and getglobalint state =
		let value = match (Lua.tostring state 1) with
			| Some name -> 
				(try getGlobalInt name
				with _ -> 0)
			| _ -> failwith "Invalid call in lua at get_global_int"
		in Lua.pushinteger state value
	and removeglobalint state =
		match (Lua.tostring state 1) with
			| Some name -> removeGlobalInt name
			| _ -> failwith "Invalid call in lua at remove_global_int"
			
	and setglobalstring state =
		match (Lua.tostring state 1),(Lua.tostring state 2) with
			| Some name,Some value -> setGlobalString name value
			| _ -> failwith "Invalid call in lua at set_global_string"
	and getglobalstring state =
		let value = match (Lua.tostring state 1) with
			| Some name -> 
				(try getGlobalString name
				with _ -> "")
			| _ -> failwith "Invalid call in lua at get_global_string"
		in Lua.pushstring state value
	and removeglobalstring state =
		match (Lua.tostring state 1) with
			| Some name -> removeGlobalString name
			| _ -> failwith "Invalid call in lua at remove_global_string"
	in 
	registerStaticFunction "print_debug" printdebug;
	
	registerStaticFunction "get_environement_var" getenvironementvar ~output:1;
	
	registerStaticFunction "set_global_int" setglobalint;
	registerStaticFunction "get_global_int" getglobalint ~output:1;
	registerStaticFunction "remove_global_int" removeglobalint;
	
	registerStaticFunction "set_global_string" setglobalstring;
	registerStaticFunction "get_global_string" getglobalstring ~output:1;
	registerStaticFunction "remove_global_string" removeglobalstring;
	
	
;;

let registerDynamicFuncLua () =
	let changeroom state =
		match (Lua.tostring state 1),(Lua.tostring state 2) with
			| Some name,Some node -> 
				let w = Jed.getWindow () in 
				w#changeRoom name node ()
			| _ -> failwith "Invalid call in lua at change_room"
	and giveitem state =
		match (Lua.tostring state 1) with
			| Some name -> 
				if not(invCheckItem name) then
					invAddItem name
			| _ -> failwith "Invalid call in lua at give_item"
	and dropitem state =
		match (Lua.tostring state 1) with
			| Some name -> 
				if invCheckItem name then
					invDropItem name
			| _ -> failwith "Invalid call in lua at drop_item"
	and addcharacter state =
		match (Lua.tostring state 1),(Lua.tostring state 2),
			(Lua.tointeger state 3),(Lua.tointeger state 4)
		with
			| (Some name,Some node,ox,oy) -> 
				let w = Jed.getWindow () in 
				w#addCharacterToDisplay name node;
				begin if (ox <> 0 || oy <> 0) then
					w#placeOffsetNode name node (ox,oy)
				end
			| _ -> failwith "Invalid call in lua at add_character"
	and placeitem state =
		match (Lua.tostring state 1),(Lua.tointeger state 2),(Lua.tointeger state 3) 
		with
			| (Some name,x,y) -> 
				let w = Jed.getWindow () in 
				w#addItemToDisplay name (x,y)
			| _ -> failwith "Invalid call in lua at place_item"
	and setanimation state =
		match (Lua.tostring state 1),(Lua.tostring state 2) with
			| (Some obj,Some anim) -> 
				let w = Jed.getWindow () in 
				w#setAnimation obj anim
			| _ -> failwith "Invalid call in lua at set_animation"
	and removeobject state =
		match (Lua.tostring state 1) with
			| Some name -> 
				let w = Jed.getWindow () in 
				w#removeDisplayElement name
			| _ -> failwith "Invalid call in lua at remove_object"
	and setdialog state =
		match (Lua.tostring state 1) with
			| (Some name) -> 
				let w = Jed.getWindow () in 
				w#setDialog name
			| _ -> failwith "Invalid call in lua at set_dialog"
	in 	
	registerDynamicFunction "change_room" changeroom;
	registerDynamicFunction "give_item" giveitem;
	registerDynamicFunction "drop_item" dropitem;
	registerDynamicFunction "add_character" addcharacter;
	registerDynamicFunction "place_item" placeitem;
	registerDynamicFunction "set_animation" setanimation;
	registerDynamicFunction "remove_object" removeobject;
	registerDynamicFunction "set_dialog" setdialog
;;

let initLua () =
	let _ = registerStaticFuncLua () in
	let _ = loadGlobalScripts (envString#get "scriptDir") in
	registerDynamicFuncLua ();
	print_string "┝┅ Scripts loaded.\n";
;;

(* Setup **********************************************************************)
let setup execDir = 
	(* motd *)
	print_string "╒═════════════════════════════╕\n";
	print_string "╎ Wins          Version 233rd ╎\n";
	print_string "╎  The Point and Click Motor  ╎\n";
	print_string "╞═════════════════════════════╛\n";
	
	(* Open Xml gobal file *)
	let xmlPath = execDir//"game.xml" in
	let xmlGame = 
		if not (Sys.file_exists xmlPath) then
			raise Not_found
		else
			(new treeXml xmlPath)#getFirstByName "game"
	in 
	
	(* Setup Environement variables *)
	let dimension = (xmlGame#getFirstByName "dimension")
	in
	envString#set "name" (xmlGame#getAttr "name");
	envString#set "icon" (xmlGame#getAttr "icon");
	envString#set "xScreen" (dimension#getAttr "x");
	envString#set "yScreen" (dimension#getAttr "y");
	envString#set "dir" execDir;
	envString#set "itemDir" (execDir//(xmlGame#getFirstByName "itemDir")
		#getAttr "href");
	envString#set "characterDir" (execDir//(xmlGame#getFirstByName "characterDir")
		#getAttr "href");
	envString#set "roomDir" (execDir//(xmlGame#getFirstByName "roomDir")
		#getAttr "href");
	envString#set "scriptDir" (execDir//(xmlGame#getFirstByName "scriptDir")
		#getAttr "href");
	envString#set "fontDir" (execDir//(xmlGame#getFirstByName "fontDir")
		#getAttr "href");
	envString#set "firstRoom" ((xmlGame#getFirstByName "firstRoom")
		#getAttr "name");
	envString#set "firstNode" ((xmlGame#getFirstByName "firstRoom")
		#getAttr "node");
	envString#set "player" ((xmlGame#getFirstByName "player")
		#getAttr "name");
	
	print_string "┝┅ Runtime variables loaded.\n";
	
	(* Setup game's objects *)
	ignore(try 
		print_string "┝┅ Load game's objects...\n";
		loadItems ();
		loadCharacters ();
		loadRooms ();
		print_string "┝┅ Game's objects loaded.\n";
		Jed.loadFonts (envString#get "fontDir");
		initLua ();
	with 
		| Failure e -> 
			print_string ("☢ Error during loading data game : "^e^" \n"); 
			Printexc.print_backtrace stdout;
			exit 2
		| e -> 
			let str = Printexc.to_string e in
			print_string ("☢ Error during loading data game of kind "^str^"\n");
			Printexc.print_backtrace stdout;
			exit 2
	); print_string "┝┅ game data loaded.\n";
	
	(* End Setup ! *)
	print_string ("╘══ "^(envString#get "name")
		^" is loaded in "^envString#get "dir"^"\n")
;;

(* Initialization *************************************************************)
let initialization execDir = 
	try 
		if not (Sys.is_directory execDir) then (
			print_string "The specified path is not a folder.\n";
			Printexc.print_backtrace stdout;
			exit 2
			)
		else 
			let _ =
				try 
					setup execDir
				with 
					| Not_found ->
						print_string 
							"☢ There is no file 'game.xml' in the directory of the game.\n";
						Printexc.print_backtrace stdout;
						exit 2
					| _ -> 
						print_string "☢ The file 'game.xml' is invalid.\n";
						Printexc.print_backtrace stdout;
						exit 2
			in flush stdout;
			try 
				main ()
			with
				| Failure e -> 
					print_string ("☢ Error during game : "^e^" \n"); 
					Printexc.print_backtrace stdout;
					exit 2
				| e -> 
					let str = Printexc.to_string e in
					print_string ("☢ Error during game of kind "^str^"\n");
					Printexc.print_backtrace stdout;
					exit 2
	with _ -> (
		if execDir = "" then 
			print_string usage_msg
		else
			print_string "The specified path is not valid.\n";
			Printexc.print_backtrace stdout;
		); exit 2
;;

let get_arguments () = 
	let arg = ref "" in
	Arg.parse ([]:((Arg.key * Arg.spec * Arg.doc) list))
		(function str -> arg := str) usage_msg;
	!arg
;;

(* Run !!! ********************************************************************)
Printexc.record_backtrace true; (*Debug trace*)
initialization (get_arguments ())
