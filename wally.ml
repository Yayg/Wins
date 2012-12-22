(* Wally - The Lua Motor
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

open Lua_api
open Tool
open Expat
open Stack

(* Exceptions *****************************************************************)

(* Types **********************************************************************)
type xmlElement = 
	| Element of string * string dictionary * xmlElement ref (* son *) 
		* xmlElement ref  (* brother *)
	| Text of string *  xmlElement ref (* brother *)
	| Void

;;

(* Objects ********************************************************************)
	class treeXml = 
	object (self) 
		val data = ref Void
		val curElement = Stack.create ()
		
		initializer
			push data curElement
		
		method private pushEndChildren element parent = 
			let Element(name,attrs,children,brothers) = parent
			in 
			let rec browser = function
				| Void                                  -> element
				| Element(name,attrs,children,brothers) -> 
					ref (Element(name,attrs,children,browser !brothers))
				| Text(str,brothers)                    ->
					ref (Text(str,browser !brothers))
			in Element(name,attrs,browser !children,brothers)
			
		
		method private pushStartElement name attrs =
			let dict = 
				let rec browser dict = function 
					| (k,v)::q -> (dict#put k v; browser dict q)
					| []       -> dict
				in browser (new dictionary) attrs; 
			in
			let element = ref (Element(name,dict,ref Void, ref Void))
			in
			let rec browser = function
				| Void            -> element
				| Element(name,attrs,children,brothers)   -> 
						self#pushEndChildren element 
						(Element(name,attrs,children,brothers))
				| Text(s,brother) -> browser (!brother)
			in browser !(top curElement)
		
		method private pushText text =
			let browser = 
				let element = ref (Text(text,Void)) 
				in 
				function
					| Element(n,atts,son,broth) -> 
						begin
							let rec browseText = function
								| Void                      -> element
								| Element(n,atts,son,broth) -> 
									ref (Element(n,atts,browseText !son,broth))
								| Text(str,broth)           ->
									(Text(str,element))
							in browseText (Element(n,atts,son,broth));
							push element curElement
						end
					| Text(str,_)                           ->
						begin
							let rec browseBrother = function
								| Void                      -> element
								| Element(n,atts,son,broth) -> 
									ref (Element(n,atts,browseText !son,broth))
								| Text(str,broth)           ->
									(Text(str,element))
							in browseBrother (Element(n,atts,son,broth));
							top curElement := Text(str, element);
							push element curElement
						end
					| _                                     ->
						top curElement := !element
			in browser !(top curElement)
				
					
					
		
	end
;;

(* Global Variables ***********************************************************)
(** Lua runtime environment. *)
let luaEnv = LuaL.newstate ();;
(** Global Counters dictionary *)
let globalCounts = new dictionary;;

(* Functions ******************************************************************)
LuaL.openlibs luaEnv;;

let registerFunction name func = 
	Lua.register luaEnv name !func
;;

let registerGlobalCount name (value:int) =
	globalCounts#put name value
;;

let updateGlobalCount name value =
	globalCounts#update name value
;;

let getGlobalCount name =
	globalCounts#get name
;;

let removeGlobalCount name =
	globalCounts#remove name
;;

let load_file file =
	let data = open_in file in
	let n = in_channel_length data in
	let s = String.create n in
	really_input data s 0 n;
	close_in data;
  (s)
;;

(*
let load_xml file =
	let data = load_file file
	and parserXml = parser_create ~encoding:(Some "UTF-8")
	in
	parse parserXml data;
	final parserXml;
	parserXml
;;
*)
	
