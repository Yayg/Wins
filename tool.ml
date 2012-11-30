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
exception Existing_entry

(* Objects ********************************************************************)
class ['a] dictionary = 
	object (self)
		val data = ref ([]:((string * 'a) list))
		
		method get key =
			let rec brower = function
				| []                    -> raise Not_found
				| (k,_)::_ when k > key -> raise Not_found
				| (k,e)::q when k = key -> e
				| _::q                  -> brower q
			in brower !data
		method keys () =
			let rec brower = function
				| []       -> []
				| (k,_)::q -> k::brower q
			in brower !data
		method elements () =
			let rec brower = function
				| []       -> []
				| (_,e)::q -> e::brower q
			in brower !data
		method is_empty () = 
			(function [] -> true | _::_ -> false) !data 
		method clear () = 
			data := []
		method length () =
			let rec brower i = function
				| []   -> i
				| _::q -> brower (i+1) q
			in brower 0 !data
		method put key element =
			let rec brower = function
				| []                    -> (key,element)::[]
				| (k,e)::q when k > key -> (key,element)::(k,e)::q
				| (k,_)::_ when k = key -> raise Existing_entry
				| c::q                  -> c::brower q
			in data := brower !data
		method update key element = 
			let rec brower = function
				| []                    -> raise Not_found
				| (k,_)::_ when k > key -> raise Not_found
				| (k,_)::q when k = key -> (key,element)::q
				| c::q                  -> c::brower q
			in data := brower !data
		method remove key =
			let rec brower = function
				| []                    -> raise Not_found
				| (k,_)::_ when k > key -> raise Not_found
				| (k,e)::q when k = key -> q
				| c::q                  -> c::brower q
			in data := brower !data
	end
;;


