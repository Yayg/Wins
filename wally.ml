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

(* Exceptions *****************************************************************)
exception Not_found
exception Broken
exception Void

(* Types **********************************************************************)
type xmlElement = 
	| BeginElement of string * string dictionary
	| EndElement of string
	| Text of string
;;

(* Objects ********************************************************************)
class treeXml = 
	object (self) 
		val data = ref ([]:(xmlElement list))
		
		method pushStartElement name attrs =
			let dict = new dictionary in
			let rec browser = function 
					| (k,v)::q -> (dict#put k v; browser q)
					| []       -> ()
			in 
			let newElement =
				browser attrs;
				BeginElement(name, dict)
			in data := newElement::!data
		method pushEndElement name =
			data := EndElement(name)::!data
		method pushText text =
			data := Text(text)::!data
		method reverse () =
			let rec invert = function
				| []     -> []
				| a :: q -> a :: (invert q)
			in invert !data
			
		method attributs () =
 			let dict =
  				let rec browser = function
   					| [] -> raise Void
   					| BeginElement(_,d)::_ -> d
  					| _::q -> browser q
  				in browser !data
 			in dict#keys ()	
		method getAttribut attr =
			let dict = 
				let rec browser = function
					| []                    -> raise Void
					| BeginElement(_,d)::[] -> d
					| _::q                  -> browser q
				in browser !data 
			in dict#get attr
		
		method copy () =
			Oo.copy self
		method private newElement (mem:(xmlElement list)) =
			let saveData = !data in 
			let element =
				data := mem;
				self#copy ()
			in
			data := saveData;
			element
		method private bodyElement (l:(xmlElement list)) =
			let rec core d = function
				| []                          -> raise Broken
				| EndElement n::q when d = 0  -> EndElement(n)::[]
				| BeginElement (n,a)::q       -> BeginElement(n,a)::core (d+1) q
				| EndElement n::q when d <> 0 -> EndElement(n)::core (d-1) q
				| e::q                        -> e::core d q
			in core 0 l
		method getElementById id =
			let rec beginBrowser = function 
				| []                                                    -> 
					raise Not_found
				| BeginElement(name, attrs)::q when attrs#get "id" = id ->
					BeginElement(name, attrs)::q
				| _::q                                                  -> 
					beginBrowser q
			in
			self#newElement (self#bodyElement (beginBrowser !data))
		method getElementsByTagName tagName = 
			let rec browser = function
				| []                                               -> []
				| BeginElement(name, attrs)::q when name = tagName ->
					self#newElement 
					(self#bodyElement (BeginElement(name, attrs)::q))
					::browser q
				| _::q                                             -> browser q
			in browser !data
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

let load_xml file =
	let data = load_file file
	and parserXml = parser_create ~encoding:(Some "UTF-8")
	in
	parse parserXml data;
	final parserXml;
	parserXml
;;
