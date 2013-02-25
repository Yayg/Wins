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
open Tool


(* Exceptions *****************************************************************)
exception Sdl_not_initialized

(* Types **********************************************************************)
type updateAction =
	| Position of (int*int)
	| Animation of rect
	| Nop
;;

(* Object *********************************************************************)
class displayUpdating window element =
	object (self)
		
		val animationUpdate = Queue.create ()
		val positionUpdate = Queue.create ()
		
		val mutable nameAnimation = "idle"
		val mutable actualSurface = load_image (element#getDir//"animation/idle.png")
		val mutable loopAnimation = false
		val mutable w = 0
		val mutable h = 0
		
		(* Draw Moving *)
		
		method getLine (c,d) (e,f) = 
			let rec line (a,b) (x,y) =
		match (a,b,x,y) with
		(* diagonales *)
		|(a,b,x,y) when (x > a)&&(y > b) -> 
			push (a,b) (positionUpdate);
			line (a + 1,b + 1) (x,y)
		|(a,b,x,y) when (x > a)&&(y < b) -> 
			push (a,b) (positionUpdate);
			line (a + 1,b - 1) (x,y)
		|(a,b,x,y) when (x < a)&&(y < b) -> 
			push (a,b) (positionUpdate);
			line (a - 1,b - 1) (x,y)
		|(a,b,x,y) when (x < a)&&(y > b) -> 
			push (a,b) (positionUpdate);
			line (a - 1,b + 1) (x,y)
		(* hauteurs *)
		|(a,b,x,y) when (x = a)&&(y > b) -> 
			push (a,b) (positionUpdate);
			line (a,b + 1) (x,y)
		|(a,b,x,y) when (x = a)&&(y < b) -> 
			push (a,b) (positionUpdate);
			line (a,b - 1) (x,y)
		|(a,b,x,y) when (x > a)&&(y = b) -> 
			push (a,b) (positionUpdate);
			line (a + 1,b) (x,y)
		|(a,b,x,y) when (x < a)&&(y = b) -> 
			push (a,b) (positionUpdate);
			line (a - 1,b) (x,y)
		|_ -> 
			push (a,b) (positionUpdate);
			in line (c,d) (e,f)
			
			(*
		method getLine (a,b) (x,y) =
			let dx = ref (x - a) 
			and dy = ref (y - b)
			and a = ref a
			and b = ref b
			and x = ref x
			and y = ref y
			in
			if (!dx <> 0) then
				begin
				if (!dx > 0) then 
					begin
					if (!dy <> 0) then
						begin
						if (!dy > 0) then
							begin
							if (!dx >= !dy) then (* 1er octant *)
								let e = ref !dx 
								in
								dx := !e * 2;
								dy := !dy * 2;
								while (!a <> !x) do
									push (!a,!b) (positionUpdate);
									begin
									a := !a + 1;
									e := !e - !dy;
									if ( !e < 0) then
										b := !b + 1 ;
										e := !e + !dx
									end
								done
							else (* 2eme octant *)
								let e = ref !dy
								in
								dx := !dx * 2;
								dy := !e * 2 ;
								while (!b <> !y) do
									push (!a,!b) (positionUpdate);
									begin
									b := !b + 1;
									e := !e - !dx;
									if (!e < 0) then
										e := !e + !dy;
										a := !a + 1;
										end
								done
							end
						else (* dy < 0 *)
							begin
							if (!dx >= (-1) * !dy) then (* 8eme octant *)
								let e = ref !dx 
								in
								dx := !e * 2;
								dy := !dy * 2;
								while (!a <> !x) do
									push (!a,!b) (positionUpdate);
									begin
									a := !a + 1 ;
									e := !e + !dy ;
									if (!e < 0) then
										b := !b - 1;
										e := !e + !dx;
									end
								done
							else (* 7eme octant *)
								let e = ref !dy
								in
								dx := !dx * 2;
								dy := !e * 2;
								while (!b <> !y) do
									push (!a,!b) (positionUpdate);
									begin
									b := !b - 1;
									e := !e + !dy;
									if (!e > 0) then
										e := !e + !dx;
										a := !a + 1;
									end
								done
							end
						end
					else (* dy = 0 *)
						while (!a <> !x) do
							a := !a + 1;
							push (!a,!b) (positionUpdate);
						done
					end
				else (* dx < 0 *)
					begin
					if (!dy <> 0) then
						begin
						if (!dy > 0) then
							begin
							if ((-1) * !dx >= !dy) then (* 4eme octant *)
								let e = ref !dx 
								in
								dx := !e * 2;
								dy := !dy * 2;
								while (!a <> !x) do
									push (!a,!b) (positionUpdate);
									begin
									a := !a - 1 ;
									e := !e + !dy;
									if (!e >= 0) then
										e := !e + !dx;
										b := !b + 1;
									end
								done
							else (* 3eme octant *)
								let e = ref !dy 
								in
								dx := !dx * 2;
								dy := !e * 2;
								while (!b <> !y) do
									push (!a,!b) (positionUpdate);
									begin
									b := !b + 1 ;
									e := !e + !dx;
									if (!e <= 0) then
										e := !e + !dy;
										a := !a - 1;
									end
								done
							end
						else (* dy < 0 *)
							begin
							if (!dx <= !dy) then (* 5eme octant *)
								let e = ref !dx
								in
								dx := !e * 2;
								dy := !dy * 2;
								while (!a <> !x) do
									push (!a,!b) (positionUpdate);
									begin
									a := !a - 1 ;
									e := !e + !dy;
									if (!e >= 0) then
										e := !e + !dx;
										b := !b - 1;
									end
								done
							else
								let e = ref !dy
								in
								dx := !dx * 2;
								dy := !e * 2;
								while (!b <> !y) do
									push (!a,!b) (positionUpdate);
									begin
									b := !b - 1;
									e := !e - !dx;
									if (!e >= 0) then
										e := !e + !dy;
										a := !a - 1;
									end
								done
							end
						end
					else (* dy = 0 *)
						while (!a <> !b) do
							push (!a,!b) (positionUpdate);
						done
					end
				end
			else
				begin
				if (!dy <> 0) then
					begin
					if (!dy > 0) then
						while (!b <> !y) do
						b := !b + 1;
						push (!a,!b) (positionUpdate);
						done
					else (* dy = 0 *)
						while (!b <> !y) do
						b := !b - 1;
						push (!a,!b) (positionUpdate);
						done
					end
				end;
			push (!x,!y) (positionUpdate)
			*)
		
		(* Update Methods *)
		method setAnimation name =
			let noper n =
				for i = 0 to (n-1) do
					push Nop animationUpdate
				done
			and animation = element#getDataAnim#getElementById name in
			let frames = (animation#getChildren ())#getElementsByName "frame" in
			let rec browser i t = function (*! Ne prend pas en compte l'orientation il faut mutiplier y par le numéro de l'orientation *)
				| [] -> ()
				| f::q -> 
					let time = int_of_string(f#getAttr "time") in
					push (Animation (rect (i*w) h w h)) animationUpdate;
					noper (time-t);
					browser (i+1) time q
			in
			actualSurface <- 
				load_image (element#getDir//"animation/"^(animation#getAttr "file"));
			loopAnimation <- bool_of_string(animation#getAttr "loop");
			w <- int_of_string(animation#getAttr "w");
			h <- int_of_string(animation#getAttr "h");
			browser 0 1 frames;
			nameAnimation <- name
		
		(* Get Actions *)
		method getActions =
			let actionAnimation =
				if is_empty animationUpdate then
					if loopAnimation then
						begin
							self#setAnimation nameAnimation;
							pop animationUpdate
						end
					else Nop
				else
					pop animationUpdate
			and actionPosition =
				if is_empty positionUpdate then
					Nop
				else
					Position (pop positionUpdate)
			in 
			(actionAnimation, actionPosition)
		method getSurface =
			actualSurface
	end
;;

(* Type ***********************************************************************)
type displayElement = {
	data : displayable;
	mutable img : rect;
	mutable pos : (int  * int);
	updating : displayUpdating
	}
;;

(* Object *********************************************************************)
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
		
		
		(** Update Display **)
		method private updataDisplayData =
			let rec browser = function
				| [] -> ()
				| element::q ->
					let (actionAnimation,actionPosition) = 
						element.updating#getActions in
					begin
						match actionAnimation with
							| Animation r -> element.img <- r
							| _ -> ()
					end;
					begin
						match actionPosition with
							| Position p -> element.pos <- p
							| _ -> ()
					end;
					browser q
			in browser (displayData#elements ())
		method private display = 
			let rec browser = function
				| [] -> ()
				| element::q -> 
					let surface = element.updating#getSurface
					and clip = element.img
					and position = element.pos
					in
					self#displayImage clip surface position;
					browser q
			in browser (displayData#elements ())
		
		(** Storing Data **)
		method setBackgroud surface =
			background <- surface
		
		method addItemToDisplay name (x,y) =
			let item = getItem name in
			let element =
				{
					data = (item :> displayable);
					img = rect 0 0 0 0; 
					pos = (x,y); 
					updating = (new displayUpdating window (item :> displayable))
				}
			in
			displayData#set name element
		method addCharacterToDisplay name (x,y) =
			let character = getCharacter name in
			let element =
				{
					data = (character :> displayable);
					img = rect 0 0 0 0;
					pos = (x,y);
					updating = (new displayUpdating window (character :> displayable))
				}
			in displayData#set name element
		
		method removeDisplayElement name =
			displayData#remove name
		method fushDisplayData () =
			displayData#clear ()
		
		(** Update Data **)
		method setAnimation objectName animationName =
			(displayData#get objectName).updating#setAnimation animationName
		method placeTo objectName newPosition =
			(displayData#get objectName).pos <- newPosition
		method moveTo objectName newPosition =
			let actualPosition = (displayData#get objectName).pos in
			(displayData#get objectName).updating#getLine actualPosition newPosition
		
		
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
		method private displayImage clip src (x,y) = 
			let dst = !window
			and dst_rect = rect x y 0 0 in
			blit_surface ~src ~src_rect:clip ~dst ~dst_rect ()
		
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
	end
;;

(* Global Variables ***********************************************************)
let window = ref None

(* Functions ******************************************************************)

let rec line (a,b) (x,y) = 
		match (a,b,x,y) with
		(* diagonales *)
		|(a,b,x,y) when (x > a)&&(y > b) -> 
			print_string(string_of_int(a)^","^string_of_int(b)); 
			print_newline();
			line (a + 1,b + 1) (x,y)
		|(a,b,x,y) when (x > a)&&(y < b) -> 
			print_string(string_of_int(a)^","^string_of_int(b)); 
			print_newline();
			line (a + 1,b - 1) (x,y)
		|(a,b,x,y) when (x < a)&&(y < b) -> 
			print_string(string_of_int(a)^","^string_of_int(b)); 
			print_newline();
			line (a - 1,b - 1) (x,y)
		|(a,b,x,y) when (x < a)&&(y > b) -> 
			print_string(string_of_int(a)^","^string_of_int(b)); 
			print_newline();
			line (a - 1,b + 1) (x,y)
		(* hauteurs *)
		|(a,b,x,y) when (x = a)&&(y > b) -> 
			print_string(string_of_int(a)^","^string_of_int(b)); 
			print_newline();
			line (a,b + 1) (x,y)
		|(a,b,x,y) when (x = a)&&(y < b) -> 
			print_string(string_of_int(a)^","^string_of_int(b)); 
			print_newline();
			line (a,b - 1) (x,y)
		|(a,b,x,y) when (x > a)&&(y = b) -> 
			print_string(string_of_int(a)^","^string_of_int(b)); 
			print_newline();
			line (a + 1,b) (x,y)
		|(a,b,x,y) when (x < a)&&(y = b) -> 
			print_string(string_of_int(a)^","^string_of_int(b)); 
			print_newline();
			line (a - 1,b) (x,y)
		|_ -> 
			print_string(string_of_int(a)^","^string_of_int(b)); 
			print_newline()

let loadImage path = 
	load_image path

let initW () =
	window := Some (new sdlWindow (int_of_string(Zak.envString#get "xScreen")) (int_of_string(Zak.envString#get "yScreen")))

let getWindow () = match !window with
			|None -> raise Sdl_not_initialized
			|Some a -> a
;;
