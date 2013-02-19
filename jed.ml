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
open Queue

open Sdl
open Sdlwm
open Sdlvideo
open Sdlloader

open Zak
open Max
open Tool


(* Exceptions *****************************************************************)
exception Sdl_not_initialized

(* Types **********************************************************************)
type movingUpAction =
	| Moving of (int*int)
	| Nop
;;

type displayElement = {
	mutable img : surface;
	mutable pos : (int  * int);
	updating : displayUpdating
	}
;;

(* Objects ********************************************************************)
class sdlWindow width height =
	object (self) 
		val window = ref (set_video_mode ~w:width ~h:height [`DOUBLEBUF])
		val mutable exLoop = None
		
		val mutable fullscreen = false
		val mutable run = true
		val mutable ticks = 0
		
		val mutable background = get_video_surface ()
		val displayData = new dictionary
		
		initializer
			set_caption (envString#get "name") (envString#get "icon");
			exLoop <- Some (Thread.create self#loop ())
		
		
		(** Update Data **)
		(*!
		method private updataDisplayData =
			let rec browser = function
				| [] -> ()
				| element::q ->
					let posUpdates = element.posUpdates in
					begin if not(is_empty posUpdates) then
						let action = pop posUpdates in
						match action with
							| Nop -> ()
							| Moving pos -> element.pos <- pos 
					end; browser q
			in browser (displayData#elements ())
		method private displayData = 
			let rec browser = function
				| [] -> ()
				| element::q -> 
					let surface = element.img
					and position = element.pos
					in
					self#displayImage surface position;
					browser q
			in browser (displayData#elements ())
		
		(** Storing Data **)
		method setBackgroud surface =
			background <- surface
		
		method addDisplayElement name surface position =
			displayData#set name 
			{img=surface; pos=position; posUpdates=(Queue.create()); imgUpdates=(new animationUpdating)}
		method removeDisplayElement name =
			displayData#remove name
		method fushDisplayData () =
			displayData#clear ()
		*)
		
		(** Window Manager **)
		method getSurface =
			!window
		method toggleFullscreen =
			fullscreen <- toggle_fullscreen ()
		method setTitle title =
			let (_,icon) = get_caption () 
			in set_caption title icon
		method setIcon icon =
			let (title,_) = get_caption ()
			in set_caption title icon
			
		
		(** Low Level Displaying **)
		method private displayImage src (x,y) = 
			let dst = !window
			and dst_rect = (rect x y 0 0) in
			blit_surface ~src ~dst ~dst_rect ()
		
		(** Loop Displaying Event **)
		method private loop () = 
			while run do
				ticks <- 17 + Sdltimer.get_ticks (); (*17 ms -> 60fps*)
				flip !window;
				
				begin match Sdlevent.poll () with
					| Some Sdlevent.QUIT -> Sdl.quit (); run <- false
					| None -> ()
					| _ -> ()
				end;
				
				while (Sdltimer.get_ticks ()) <= ticks do () done
			done
			
		(** Print Error **)

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
