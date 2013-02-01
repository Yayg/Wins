(* Jed - Display motor
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

open Thread
open Sdl
open Sdlvideo
open Sdlwm
open Zak
open Sdlloader


(* Exceptions *****************************************************************)
exception Sdl_not_initialized

(* Objects ********************************************************************)
class sdlWindow width height =
	object (self) 
		val window = ref (set_video_mode ~w:width ~h:height [`DOUBLEBUF])
		val mutable exLoop = None
		
		val mutable fullscreen = false
		val mutable run = true
		
		initializer
			set_caption (envString#get "name") (envString#get "icon");
			exLoop <- Some (create self#loop ())

		method getSurface =
			!window
		method toggleFullscreen () =
			fullscreen <- toggle_fullscreen ()
		method setTitle title =
			let (_,icon) = get_caption () 
			in set_caption title icon
		method setIcon icon =
			let (title,_) = get_caption ()
			in set_caption title icon
		method displayImage src x y = 
			let dst = !window 
			and dst_rect = (rect x y 0 0) in
			blit_surface ~src ~dst ~dst_rect ()
		method move image a b = 
			let dst_rect = rect a b 0 0 in
			set_clip_rect image dst_rect
		method update rect image =
			update_rect ~rect image
		method loop () = 
			while run do
				flip !window;
				begin match Sdlevent.poll () with
					| Some Sdlevent.QUIT -> Sdl.quit (); run <- false
					| None -> ()
					| _ -> ()
				end;
				delay 0.016666667 (*60 fps*)
			done
	end
;;

(* Global Variables ***********************************************************)
let window = ref None

(* Functions ******************************************************************)

let loadImage path = 
	load_image path

let point x y = 
	rect x y 0 0

let initW () =
	window := Some (new sdlWindow (int_of_string(Zak.envString#get "xScreen")) (int_of_string(Zak.envString#get "yScreen")))

let getWindow () = match !window with
			|None -> raise Sdl_not_initialized
			|Some a -> a
;;
