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
		invAddItem "duck";
		invAddItem "hammer";
		w#changeRoom (envString#get "firstRoom")
	in
	Woop.bigLoop();
	exit 0
;;

(* Registration function Lua **************************************************)
let registerStaticFuncLua () =
	(** print_debug (string) : print a debug message in the console **)
	let printdebug state =
		match (Lua.tostring state 1) with
			| Some s -> print_debug ("Script : "^s); 0
			| _ -> 2
	in 
	registerGlobalFunction "print_debug" printdebug;
;;

let registerDynamicFuncLua () =
	let changeroom state =
		match (Lua.tostring state 1) with
			| Some name -> 
				let w = Jed.getWindow () in 
				w#changeRoom name; 0
			| _ -> 2
	in 	
	registerGlobalFunction "change_room" changeroom;
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
			print_string ("☢ Error during loading data game : "^e^" \n"); exit 2
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
			main ()
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
