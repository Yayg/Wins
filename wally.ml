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

open Char
open String
open Lua_api
open Tool
open Expat
open Stack
open List
open Array

(* Exceptions *****************************************************************)
exception BadStyleXmlTree of string
exception IsNotXmlText
exception IsNotXmlElement
exception AttrNotFound
exception Not_found

(* Types **********************************************************************)

type len = Finite of float | Infinite

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
			push (Node(element, VoidTree, VoidTree)) stack
			(*Debug*;print_string ("pushStartElement : "^name^"\n")*)
		method private pushText stack text =
			if (text <> "")&&(text <> "\n")&&(text <> " ")&&(text <> "	") then
			(*Debug*print_string ("pushText : "^text^"\n");*)
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
			push !currentNode stack
			(*Debug*;print_string ("pushEndElement : "^name^"\n")*)
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
			(*Debug*
			;let getNameElement node = 
				let getElement = function
					| VoidTree -> 
						raise (BadStyleXmlTree 
						"VoidTree in ending element (a closing tag is alone ?)")
					| Node(element, _, _) -> element
				and getName = function
					| Text _ -> ""
					| Element (name, _) -> name
				in getName (getElement node)
			in print_string ("Current node: "^getNameElement data^"\n")
			*)
		
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
		method getAttr name =
			let element = self#getXmlElement () in
			element#getAttr name
			
		method getElementById idName =
			let rec browser = function
				| VoidTree -> VoidTree
				| Node(Element(str, dict), brother, children) when 
                                        try String.lowercase (dict#get "id") = 
						String.lowercase idName with Not_found -> false
                                        -> Node(Element(str, dict), brother, children)
				| Node(_, brother, children) ->
					let resultChildren = browser children in
					if resultChildren = VoidTree then
						browser brother
					else
						resultChildren
			in
			let result = browser data in
			if result = VoidTree then
				raise Not_found
			else
				self#newXmlTree result
		method getElementsByName name =
			let result = ref [] in
			let rec browser = function
				| VoidTree -> ()
				| Node(Element(str, dict), brother, children) when String.lowercase str = String.lowercase name ->
					let e = self#newXmlTree (Node(Element(str, dict), brother, children)) in
					result := e::!result;
					browser children;
					browser brother
				| Node(_, brother, children) -> 
					browser children;
					browser brother
			in browser data; !result
		method getFirstByName name =
			let rec browser = function
                                | VoidTree -> VoidTree
                                | Node(Element(str, dict), brother, children) 
					when String.lowercase str = String.lowercase name ->
					Node(Element(str, dict), brother, children)
                                | Node(_, brother, children) ->
                                        let resultChildren = browser children in
                                        if resultChildren = VoidTree then
                                                browser brother
                                        else
                                                resultChildren
                        in
                        let result = browser data in
                        if result = VoidTree then
                                raise Not_found
                        else
                                self#newXmlTree result
	end
;;

class ['a] graphMove xmlFile = (* test: let w = new graphMove "./game/rooms/begin/graph.xml";; *)
	object (self)
		
		val mutable tree = new treeXml (xmlFile)
		val mutable nodes = []
		val mutable distance = new dictionary (* (name,(p,((link,distance) list)))) dictionary *)
		val mutable links = new dictionary (* (((x,y),links list)) dictionary *)
		val mutable keys = []
		val mutable n = ref (-1)
		
			initializer
			self#getNodes;
			tree <- tree#getChildren ();
			self#init;
			self#dicoFusion
		
(* Initialisation des graphs **************************************************)
		
		method init =
			let rec browser = function
				| t when t#getElementsByName "node" = [] -> ()
				| t ->
				begin
					let attrs = (t#getXmlElement ())#getAttrs () in
					 match (attrs) with
						| key :: l :: x :: y :: [] ->
							let element =
							(
							(int_of_string(t#getAttr(x)),int_of_string(t#getAttr(y))),
							self#strParse (t#getAttr(l))
							)
							in
							links#set (t#getAttr(key)) element;
							browser (t#getNextBrother ())
						| _ ->  raise Not_found
				end
			in
			browser tree
			
		method dicoFusion =
			let keys = links#keys in
			let rec browser = function
				| [] -> ()
				| h :: t ->
					begin
						let (d,link) = (links#get h) in
						self#addE h link d;
						browser t
					end
			in 
			browser (keys ())
			
		method addE name l (x,y) = (* (name,((x,y),links list)) dictionary *)
			let rec browser point = function
				| [] -> []
				| h :: t ->
					begin
						let (p,_) = (links#get h) in
						((h,(self#getDistance point p)) :: (browser point t))
					end
			in 
			n := !n + 1;
			distance#set name (!n,(browser (x,y) l))
			
		method getDistance (x,y) (a,b) =
			let d1 = (y-b) and d2 = (x-a) in
			let d = float_of_int((d1 * d1) + (d2 * d2)) in 
			sqrt(d)
			
		method initDistance =
			let rec coor = function
				| [] -> []
				| (c,_) :: t -> c :: coor t
			in
			let rec browser = function
				| ([],[]) -> []
				| (a::b,c::d) -> (a,c) :: browser (b,d)
				| (_,_) -> 
					print_string("Error : Xml file not correctly written. You should have coordinates for each point.");
					raise AttrNotFound
			in 
			browser (links#keys (),coor (links#elements ()))
			
		method strParse str =
			begin
				let i = ref (String.length str - 1) and acc = ref "" and final = ref [] in
				while (0 <= !i) do
					(if (str.[!i] = ',') then
						(final := !acc :: !final;
						acc := "")
					else
						acc := Char.escaped(str.[!i]) ^ !acc);
					i := !i - 1
				done;
				final := !acc :: !final;
				!final
			end
		method getNodes =
			print_string(((tree#getFirstByName "graph")#getXmlElement ())#getName ()^"\n"); (* test de conformité du fichier graph *)
			nodes <- tree#getElementsByName "node"
		method getFirst =
			let n = nodes in
			match n with
				| [] -> tree
				| h::t -> h
		method getName =
			tree#getAttr ((tree#getXmlElement ())#getName ())
		method displayNodes = 
			nodes
		
		method getD =
			(distance : (int * (string * float) list) dictionary)
		method getLinks =
			links
		method getCoor name =
			let (x,_) = links#get name in x
			
		method getId name = 
			let rec browser (n:string) = function
				| [] -> raise Not_found
				| ((i:int),h) :: t when h = n -> i
				| _ :: t -> browser n t
			in browser name keys
			
		method initKeys =
			keys <- (let rec browser i = function 
						| [] -> []
						| h::t -> let (p,_) = distance#get h in
							(p,h) :: (browser (i+1) t)
					in browser 0 (distance#keys ()));
		method getKey =
			keys
		
		method initMatrix = 
			Array.make ((List.length keys)) (Array.make ((List.length keys)) Infinite)
			
		method displayArray a =
			let rec browser a = function
				| i when i = (Array.length a) -> ()
				| i -> 
					begin
					match a.(i) with
						| Finite(n) ->
							print_string(string_of_float(n)^"\n")
						| Infinite ->
							print_string("Infinite\n")
					end;
					browser a (i+1)
			in
			browser a 0
			
		method insert mat l = 
			let rec ins m = function
				| [] -> Array.copy m
				| (name,d) :: t-> 
					m.(self#getId name) <- Finite(d);
					ins (Array.copy m) t
			in
			ins (Array.copy mat) l
			
		method graphToMatrix =
			self#initKeys;
			let mat = self#initMatrix in
			let rec browser m l =
				print_string("\n");
				match l with
				| [] -> m
				| (i,h) :: t ->
					let (_,link) = (distance#get h) in
					m.(i) <- (Array.copy (self#insert (m.(i)) link));
					print_string(string_of_int(i)^"\n");
					self#displayArray m.(i);
					browser m t
			in self#endMatrix(browser mat keys)
			
		method endMatrix ma = 
			let rec endM m = function
				| n when n = Array.length m -> m
				| n -> (m.(n).(n) <- Finite(0.));
					endM (Array.copy m) (n+1)
			in endM ma 0
(* Dijkstra *******************************************************************)

		method minArray a =
			let rec browser v min a = function
				| i when i = Array.length a -> v
				| i ->
					(
					match a.(i) with
						| Finite(n) when ((match min with Finite(m) -> n < m | _ -> true)&&(n <> (0.))) -> browser v (Finite(n)) a (i+1)
						| _ -> browser v min a (i+1)
					)
			in browser 0 Infinite a 0
				
		

	end
(* Global Variables ***********************************************************)
(** Lua runtime environment. *)
let luaEnv = LuaL.newstate ();;

(* Functions ******************************************************************)
let test h =
	let w = new graphMove "./game/rooms/begin/graph.xml" in
	w#graphToMatrix
;;
LuaL.openlibs luaEnv;;

let registerFunction name func = 
	Lua.register luaEnv name !func
;;

