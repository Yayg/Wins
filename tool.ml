(* Tool - Tool Object And Function
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

(* Exceptions *****************************************************************)
exception Not_found

(* Types **********************************************************************)
type ('a) binaryTree =
	| Node of 'a * ('a) binaryTree * ('a) binaryTree
	| VoidTree
;;

type 'a graph = 
	| Point of 'a * 'a graph list
	| Void
;;

(* Objects ********************************************************************)
class ['a] dictionary = 
	object (self)
		val data = ref ([]:((string * 'a) list))
		
		method get key =
			let rec browser = function
				| []                    -> raise Not_found
				| (k,_)::_ when k > key -> raise Not_found
				| (k,e)::q when k = key -> e
				| _::q                  -> browser q
			in browser !data
		method keys () =
			let rec browser = function
				| []       -> []
				| (k,_)::q -> k::browser q
			in browser !data
		method elements () =
			let rec browser = function
				| []       -> []
				| (_,e)::q -> e::browser q
			in browser !data
		method is_empty () = 
			(function [] -> true | _::_ -> false) !data 
		method clear () = 
			data := []
		method length () =
			let rec browser i = function
				| []   -> i
				| _::q -> browser (i+1) q
			in browser 0 !data
		method set key element =
			let rec browser = function
				| []                    -> (key,element)::[]
				| (k,e)::q when k > key -> (key,element)::(k,e)::q
				| (k,_)::q when k = key -> (key,element)::q
				| c::q                  -> c::browser q
			in data := browser !data
		method remove key =
			let rec browser = function
				| []                    -> raise Not_found
				| (k,_)::_ when k > key -> raise Not_found
				| (k,e)::q when k = key -> q
				| c::q                  -> c::browser q
			in data := browser !data
	end
;;


(* Functions ******************************************************************)
let load_file file =
	let data = open_in file in
	let n = in_channel_length data in
	let s = String.create n in
	really_input data s 0 n;
	close_in data;
  (s)
;;

let cut str =
	let t = String.length str
	and (i,l) = (ref 0, ref 0)
	and o = ref ([]:string list)
	in
	while !i < t do
		(match str.[!i] with
			| ' ' | '\012' | '\n' | '\r' | '\t' -> 
				o := String.sub str !l (!i-(!l))::!o;
				l := !i+1
			| _ when !i = t-1 -> 
				o := String.sub str !l (t-(!l))::!o
			| _ -> ()
		); i := !i + 1
	done;
	
	!o
;;
	
let regexp str exp =
	let re = Str.regexp_case_fold exp in
	try 
		ignore(Str.search_forward re str 0);
		true
	with _-> false
;;

let (//) dir path =
	if not (regexp dir "/$") then
		dir^"/"^path
	else
		dir^path
;;
