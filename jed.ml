(* Jed - Display motor
################################################################################
#    Wins is a "Point and Click" Game Motor written with OCaml                 #
#    Copyright (C) 2013    Philémon Gardet [philemon.gardet@epita.fr]          #
#                          Rafaël Gozlan [rafael.gozlan@epita.fr]              #
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

open Sdl
open Sdlvideo
open Sdlwm


(* Objects ********************************************************************)
class sdlWindow width height =
	object (self) 
		val window = ref (set_video_mode ~w:width ~h:height [`DOUBLEBUF])
		val fullscreen = ref false
		
		method get_surface () =
			!window
		method toggle_fullscreen () =
			fullscreen := toggle_fullscreen ()
		method set_title title =
			let (_,icon) = get_caption () 
			in set_caption title icon
		method set_icon icon =
			let (title,_) = get_caption ()
			in set_caption title icon
	end
;;

(* Global Variables ***********************************************************)


(* Functions ******************************************************************)

