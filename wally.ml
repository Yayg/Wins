(* Wally - The Lua/Data Motor
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

(* Types **********************************************************************)

type len = Finite of float | Infinite

type xmlElementType = 
	| Element of string * string dictionary
	| Text of string

type matrix = Matrix of len array array | VoidM
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
	method checkAttr name =
		match data with
			| Text(_) -> raise IsNotXmlElement
			| Element(_, dict) -> dict#check name
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
					(try 
						String.lowercase (dict#get "id") = String.lowercase idName 
					with _ -> false)
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
				| Node(Element(str, dict), brother, children) 
					when String.lowercase str = String.lowercase name ->
						let e = self#newXmlTree (Node(Element(str, dict), brother, children)) 
						in
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

class graph (graphXml:treeXml) = 
	object (self)
		
		val distance = new dictionary (* (name,(p,(link,distance) list)) dictionary *)
		val links = new dictionary (* ((x,y),links list) dictionary *)
		
		val n = ref (-1)
		val mutable keys = []
		val mutable aux = VoidM
		
		val changingRoom = new dictionary
		val mutable currentNode = ""
		
		initializer
			let _ =
				let nodes = (graphXml#getFirstByName "graph")#getChildren () in
				let rec browser = function
					| [] -> ()
					| e::q -> let data = e#getXmlElement () in
						let id = data#getAttr "id"
						and x = int_of_string (data#getAttr "x")
						and y = int_of_string (data#getAttr "y")
						and l = self#strParse (data#getAttr "l")
						in 
						links#set id ((x,y),l);
						begin if (data#checkAttr "swr") then
							let swr = data#getAttr "swr"
							and swn = data#getAttr "swn"
							in changingRoom#set id (swr,swn)
						end; browser q
				in browser (nodes#getElementsByName "node")
			in
			self#calculateDistances;
			aux <- Matrix(self#graphToMatrix)
			
		(** Initialisation des graphs **)
		method private strParse str =
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
		
		method private calculateDistances =
			let rec browser = function
				| [] -> ()
				| h :: t ->
					begin
						let (d,link) = (links#get h) in
						self#addE h link d;
						browser t
					end
			in 
			browser (links#keys ())
			
		method private addE name l (x,y) = (* (name,((x,y),links list)) dictionary *)
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
		
		method private graphToMatrix =
			self#initKeys;
			let mat = self#initMatrix in
			let rec browser m l =
				match l with
				| [] -> m
				| (i,h) :: t ->
					let (_,link) = (distance#get h) in
					m.(i) <- (Array.copy (self#insert (m.(i)) link));
					browser m t
			in self#endMatrix(browser mat keys)
		
		method private getDistance (x,y) (a,b) =
			let d1 = (y-b) and d2 = (x-a) in
			let d = float_of_int((d1 * d1) + (d2 * d2)) in 
			sqrt(d)
			
		method private initDistance =
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
			
		method private getD =
			(distance : (int * (string * float) list) dictionary)
		method private getLinks =
			links
			
		method private getId name = 
			let rec browser (n:string) = function
				| [] -> raise Not_found
				| ((i:int),h) :: t when h = n -> i
				| _ :: t -> browser n t
			in browser name keys
			
		method private initKeys =
			keys <- (let rec browser i = function 
						| [] -> []
						| h::t -> let (p,_) = distance#get h in
							(p,h) :: (browser (i+1) t)
					in browser 0 (distance#keys ()));
		method private getKey =
			keys
		
		method private initMatrix = 
			Array.make ((List.length keys)) (Array.make ((List.length keys)) Infinite)
			
		method private displayArray a =
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
			browser a 0;
			print_string("\n")

		method diplayM = match aux with Matrix(m) -> Array.iter self#displayArray m; print_string("\n\n") | VoidM -> ()
			
		method private insert mat l = 
			let rec ins m = function
				| [] -> Array.copy m
				| (name,d) :: t-> 
					m.(self#getId name) <- Finite(d);
					ins (Array.copy m) t
			in
			ins (Array.copy mat) l
			
		method private endMatrix ma = 
			let rec endM m = function
				| n when n = Array.length m -> m
				| n -> (m.(n).(n) <- Finite(0.));
					endM (Array.copy m) (n+1)
			in endM ma 0
			
		(** Dijkstra **)
		method dijkstra x y = 
			let rec browser y pcc avoir result =
				if (avoir <> []) then
					begin
					match avoir with
					| (_,cur) :: t ->
					if (cur = y) then
						begin
						let result = (cur :: pcc) :: result in
						if (t <> []) then
							begin
							match (t,pcc) with 
							| ((father,_) :: _ ,(prev :: _)) ->
							let p = ref prev in
							let au = ref pcc in
							while (!p <> father) do
								match !au with
								| _ :: h :: t ->
								au := h :: t;
								p := h
								| _ -> failwith "Invalid position."
							done;
							browser y (!au) t result
							| _ -> failwith	"No way to go."
							end
						else
						browser y [] t result
						end
					else
						let (_,sons) = links#get cur in
						let k = self#sub cur (self#check sons pcc) in
						if (k <> []) then
							let t = List.append k t in
							let pcc = cur :: pcc in
							browser y pcc t result
						else
							if (t <> []) then
							begin
							match (t,pcc) with 
							| ((father,_) :: _ ,(prev :: _)) ->
							let p = ref prev in
							let au = ref pcc in
							while (!p <> father) do
								match !au with
								| _ :: h :: t ->
								au := h :: t;
								p := h
								| _ -> failwith "Invalid position."
							done;
								browser y (!au) t result
							| _ -> failwith	"42"
							end
							else
								browser y [] t result

					| _ -> failwith	"42"
					end
				else
				result
			in
			browser y [x] (self#sub x (let (_,l) = links#get x in l)) []
			
		method private sub x l =
			let rec browser = function
				| [] -> []
				| h :: t -> (x,h) :: browser t
			in
			browser l
		
		method private check l1 l2 = 
			let rec browser = function
				| [] -> []
				| h :: t when List.mem h l2 -> browser t
				| h :: t -> h :: browser t
			in
			browser l1

		method private wDistance l =
			match aux with Matrix(m) ->
			let rec browser d = function
				| [] -> failwith "Impossible case."
				| a :: [] -> d
				| a :: b :: t -> 
					browser (
					d +. (match (m.(self#getId a).(self#getId b)) with 
						| Finite(x) -> x 
						| Infinite -> match (m.(self#getId b).(self#getId a)) with 
							| Finite(x) -> x
							| Infinite -> failwith "Vers l'infini et au delakjnaegnbzerbpbnepeo, !"
					)) (b :: t)
			in 
			browser 0. l
			| VoidM -> failwith "Error during matrix initialization !"
			
		(** Get Way **)
		method shorthestPath ?src:(s=currentNode) dst =
			let ways = self#dijkstra s dst in
			let rec browser w dmin = function
				| [] -> w
				| h :: t when dmin = (-1.) -> browser h (self#wDistance h) t 
				| h :: t when (self#wDistance h) < dmin -> browser h (self#wDistance h) t
				| _ :: t -> browser w dmin t 
			in 
			let track = browser [] (-1.) ways
			in if track <> [] then
				(currentNode <- dst;
				List.rev track)
			else
				[]
		
		method debug l = 
			print_string("[");
			let rec browser = function
				| [] -> print_string("]")
				| h :: [] -> print_string(h^"]\n")
				| h :: t -> print_string(h^",");
							browser t
			in browser l

		method getNearestNode (x,y) = (* (((x,y),links list)) dictionary *)
			let nearestNode = match (links#keys ()) with
				| [] -> failwith "Graph Error: Not  initialised."
				| a :: t -> let d = self#getDistance (self#getCoor a) (x,y) in 
					let rec browser node min = function
						| [] -> node 
						| h :: z when min < (self#getDistance (self#getCoor h) (x,y)) -> 
							browser node min z
						| h :: z -> browser h (self#getDistance (self#getCoor h) (x,y)) z 
					in browser a d t
			in if (self#getDistance (self#getCoor nearestNode) (x,y)) > 180. then ""
			else nearestNode
		
		(** Get Nodes Info **)
		method nodes =
			links#keys ()
		
		method getCoor name =
			let (pos,_) = links#get name in pos
		
		method getCurrentNode =
			currentNode
			
		method setCurrentNode name =
			currentNode <- name
		
		method needChangeRoom =
			if changingRoom#check currentNode then
				Some (changingRoom#get currentNode)
			else
				None
	end

(* Global Variables ***********************************************************)
(** Lua runtime environment. *)
let staticFunctions = ref []
let dynamicFunctions = ref []

let globalScripts = ref []

(* Functions ******************************************************************)
let registerStaticFunction name ?output:(n=0) func =
	let f state = ignore(func state); n
	in staticFunctions := (name,f)::!staticFunctions
;;

let registerDynamicFunction name ?output:(n=0) func =
	let f state = ignore(func state); n
	in dynamicFunctions := (name,f)::!dynamicFunctions
;;

let addGlobalScript path =
	globalScripts := path::!globalScripts
;;

(* Class **********************************************************************)
class luaRuntime =
	object (self)
		val env = LuaL.newstate ()
		
		initializer
			let rec registreFunctions = function
				| [] -> ()
				| (n,f)::q -> 
					Lua.register env n f;
					registreFunctions q
			in let rec registreScripts = function
				| [] -> ()
				| p::q -> (if not (LuaL.dofile env p) 
					then failwith ("fail to execute "^p))
			in
			LuaL.openlibs env;
			registreFunctions !staticFunctions;
			registreScripts !globalScripts;
			registreFunctions !dynamicFunctions
			
		
		method registreFunction name func =
			Lua.register env name func
		method doLine str =
			LuaL.dostring env str 
		method addScript path =
			LuaL.dofile env path

	end
;;

let newLua scriptPath =
	let l = new luaRuntime in
	ignore(l#addScript scriptPath);
	l
;;

(* InitGlobal Lua Runtime ****************************************************)
let loadGlobalScripts scriptDir =
	let elements = Array.to_list (Sys.readdir scriptDir) in
	let rec browser = function
		| [] -> ()
		| f::q when Sys.file_exists ((Filename.concat scriptDir) f) ->
			ignore(addGlobalScript (scriptDir//f)); browser q
		| _::q -> browser q
	in browser elements
;;
