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
#    Lorry "Bardaf" Guedj [lorry.guedj@epita.fr]                               #
#    Alexandre "Stawayld" Starck [alexandre.starck@epita.fr]                   #
#                                                                              #
################################################################################
#    Wins is a "Point and Click" Game Motor written with OCaml                 #
#    Copyright (C) 2013    Philémon Gardet [philemon.gardet@epita.fr]          #
#                          Rafael Gozlan [rafael.gozlan@epita.fr]              #
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

open Wally
open Zak
open Tool

let usage_msg = "Usage : wins [gameFolder]\n";;

let main () = ();;

let setup execDir = 
	(* motd *)
	print_string "╒═════════════════════════════╕\n";
	print_string "╎ Wins     Version alpha 3.14 ╎\n";
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
	let dimension = ((xmlGame#getFirstByName "dimension")#getXmlElement ())
	in
	envString#set "name" ((xmlGame#getXmlElement ())#getAttr "name");
	envString#set "icon" ((xmlGame#getXmlElement ())#getAttr "icon");
	envString#set "xScreen" (dimension#getAttr "x");
	envString#set "yScreen" (dimension#getAttr "y");
	envString#set "dir" execDir;
	envString#set "itemDir" (execDir//((xmlGame#getFirstByName "itemDir")#getXmlElement ())#getAttr "href");
	envString#set "characterDir" (execDir//((xmlGame#getFirstByName "characterDir")#getXmlElement ())#getAttr "href");
	envString#set "roomDir" (execDir//((xmlGame#getFirstByName "roomDir")#getXmlElement ())#getAttr "href");
	envString#set "fontDir" (execDir//(xmlGame#getFirstByName "fontDir")#getAttr "href");
	
	print_string "┝┅ Runtime variables loaded.\n";
	
	(* Setup Item and Character object *)
	(try 
		loadItems ();
		loadCharacters ();
		Jed.loadFonts (envString#get "fontDir");
	with 
		| Failure e -> print_string ("☢ Error during loading data game : "^e^" \n"); exit 2
		| e -> 
			let str = Printexc.to_string e in
			print_string ("☢ Error during loading data game of kind "^str^"\n");
			exit 2
	); print_string "┝┅ game data loaded.\n";
	
	(* End Setup ! *)
	print_string ("╘══ "^(envString#get "name")^" is loaded in "^envString#get "dir"^"\n")
;;
let get_arguments () = 
	let arg = ref "" in
	Arg.parse ([]:((Arg.key * Arg.spec * Arg.doc) list)) (function str -> arg := str) usage_msg;
	!arg
;;
let initialization execDir = 
	try 
		if not (Sys.is_directory execDir) then (
			print_string "The specified path is not a folder.\n";
			exit 2
			)
		else 
			(try 
				setup execDir;
			with 
				| Not_found ->
					print_string "☢ There is no file 'game.xml' in the directory of the game.\n";
					exit 2
				| _ -> 
					print_string "☢ The file 'game.xml' is invalid.\n";
					exit 2
			);
			main ()
	with _ -> (
		if execDir = "" then 
			print_string usage_msg
		else
			print_string "The specified path is not valid.\n"
		); exit 2
;;

(* Run !!! *)
initialization (get_arguments ())
