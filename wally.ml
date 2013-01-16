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
exception BadStyleXmlTree of string
exception IsNotXmlText
exception IsNotXmlElement
exception AttrNotFound

(* Types **********************************************************************)
type xmlElementType = 
	| Element of string * string dictionary
	| Text of string
;;

(* Objects ********************************************************************)
class xmlElement dataXml =
	object (self)
		val data = dataXml
	
	method getType =
		let xmlType = function
			| Text(_) -> "Text"
			| Element(_) -> "Element"
		in xmlType data
	method getString () =
		let str = function
			| Text(strg) -> strg
			| Element(_) -> raise IsNotXmlText
		in str data
	method getName () = 
		let name = function
			| Text(_) -> raise IsNotXmlElement
			| Element(name, _) -> name
		in name data
	
	method getAttrs () =
		let attrs = function
			| Text(_) -> raise IsNotXmlElement
			| Element(_, dict) -> dict#keys ()
		in attrs data
	method getAttr name =
		let attr = function
			| Text(_) -> raise IsNotXmlElement
			| Element(_, dict) -> try dict#get name with Not_found -> raise AttrNotFound
		in attr data
end
;;

class treeXml xmlFile = 
	object (self) 
		(* BinaryTree(xmlElement, BrotherTree, ChildrenTree) *)
		val mutable data = VoidTree
		
		initializer
			let xmlFile = load_file xmlFile
			and parser = parser_create ~encoding:(Some "UTF-8")
			and stack = Stack.create ()
			
			in
			set_start_element_handler parser (self#pushStartElement stack);
			set_end_element_handler parser (self#pushEndElement stack);
			set_character_data_handler parser (self#pushText stack);
			
			parse parser xmlFile;
			final parser;
			self#endParsing stack
		
		method private pushStartElement stack name attrs =
			let element = 
				let rec browser dict = function 
					| (k,v)::q -> (dict#set k v; browser dict q)
					| [] -> dict
				in Element(name, browser (new dictionary) attrs)
			in 
			push (Node(element, VoidTree, VoidTree)) stack;
			print_string ("pushStartElement : "^name^"\n")
		method private pushText stack text =
			print_string ("pushText : "^text^"\n");
			push (Node(Text(text), VoidTree, VoidTree)) stack
		method private pushEndElement stack name =
			let getNameElement node = 
				let getElement = function
					| VoidTree -> 
						raise (BadStyleXmlTree 
						"VoidTree in ending element (a closing tag is alone ?)")
					| Node(element, _, _) -> element
				and getName = function
					| Text _ -> ""
					| Element (name, _) -> name
				in getName (getElement node)
			and setBrotherTree brother = function
				| VoidTree -> 
					raise (BadStyleXmlTree 
					"Attempted assignment of a brother in an Voidtree during ending element")
				| Node(element, _, children) -> Node(element, brother, children)
			and setChildrenTree children = function
				| VoidTree ->
					raise (BadStyleXmlTree 
					"Attempted assignment of a children in an Voidtree during ending element")
				| Node(element, brother, _) -> Node(element, brother, children)
			
			and previousNode = ref VoidTree
			and currentNode = ref (pop stack)
			
			in 
			while getNameElement !currentNode <> name do
				previousNode := (setBrotherTree !previousNode !currentNode);
				currentNode := (pop stack)
			done;
			currentNode := setChildrenTree !previousNode !currentNode;
			push !currentNode stack;
			print_string ("pushEndElement : "^name^"\n")
		method private endParsing stack =
			let setBrotherTree brother = function
				| VoidTree -> 
					raise (BadStyleXmlTree 
					"Attempted assignment of a brother in an Voidtree during ending parsing")
				| Node(element, _, children) -> Node(element, brother, children)
			
			and previousNode = ref VoidTree
			and currentNode = ref (pop stack)
			
			in
			while not (is_empty stack) do
				previousNode := !currentNode;
				currentNode := pop stack;
				currentNode := setBrotherTree !previousNode !currentNode
			done;
			data <- !currentNode
		
		method private newXmlTree newData =
			let saveData = data in
			let xmlTree = 
				data <- newData;
				self#copy ()
			in
			data <- saveData;
			xmlTree
		
		method copy () =
			Oo.copy self
		method is_empty () =
			if data = VoidTree then true 
			else false
		
		method getChildren () =
			let children = function
				| Node(_, _, children) -> children
				| VoidTree -> VoidTree
			in self#newXmlTree (children data)
		method getNextBrother () =
			let brother = function
				| Node(_, brother, _) -> brother
				| VoidTree -> VoidTree
			in self#newXmlTree (brother data)
		method getIthBrother i =
			let rec browser i = function
				| VoidTree -> VoidTree
				| node when i = 0 -> node
				| Node(_, brother, _) -> browser (i-1) brother
			in self#newXmlTree (browser i data)
		
		method getXmlElement () =
			let getElement = function
				| VoidTree -> raise (BadStyleXmlTree "getXmlElement with a VoidTree")
				| Node(element, _, _) -> element
			in new xmlElement(getElement data)
			
		method getElementById idName =
			let rec browser = function
				| Node(Element(str, dict), brother, children) when 
					try dict#get "id" = idName with Not_found -> false
					-> Node(Element(str, dict), brother, children)
				| Node(_, _, children) when children <> VoidTree -> browser children
				| Node(_, brother, _) when brother <> VoidTree -> browser brother
				| _ -> raise Not_found
			in self#newXmlTree (browser data)
		method getElementsByName name =
			let rec browser = function
				| Node(Element(str, dict), brother, children) when str = name ->
					begin
						match (brother, children) with
							| _, children when children <> VoidTree -> 
								self#newXmlTree (Node(Element(str, dict), brother, children))::browser children
							| brother, _ when brother <> VoidTree ->
								self#newXmlTree (Node(Element(str, dict), brother, children))::browser brother
							| _ -> self#newXmlTree (Node(Element(str, dict), brother, children))::[]
					end
				| Node(_, _, children) when children <> VoidTree -> browser children
				| Node(_, brother, _) when brother <> VoidTree -> browser brother
				| _ -> []
			in browser data
		method getFirstByName name =
			let rec browser = function
				| Node(Element(str, dict), brother, children) when str = name ->
					Node(Element(str, dict), brother, children)
				| Node(_, _, children) when children <> VoidTree -> browser children
				| Node(_, brother, _) when brother <> VoidTree -> browser brother
				| _ -> raise Not_found
			in self#newXmlTree (browser data)
	end
;;

(* Global Variables ***********************************************************)
(** Lua runtime environment. *)
let luaEnv = LuaL.newstate ();;

(* Functions ******************************************************************)
LuaL.openlibs luaEnv;;

let registerFunction name func = 
	Lua.register luaEnv name !func
;;

