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

(* Globals Variables **********************************************************)
let window = ref None
let fonts = new dictionary
let colors = new dictionary

(* Types **********************************************************************)
type updateAction =
	| Position of (int*int)
	| Animation of rect
	| Nop
;;

type tirade = {
	emitter : string;
	mutable time : int;
	offs : (int * int);
	text : string
	}
;;
(* Object *********************************************************************)
class dialog (xmlDialog:Wally.treeXml) id =
	object (self)
	val buffer = Queue.create ()
	val image = Queue.create ()
	val mutable computing = None
	
	initializer
		let rec browser = function
			| e when e#is_empty () -> ()
			| e -> 
				let transmitter = (e#getXmlElement ())#getName ()
				and t = int_of_string((e#getAttr "time"))
				and x = int_of_string(e#getAttr "x")
				and y = int_of_string(e#getAttr "y")
				and str = ((e#getChildren ())#getXmlElement ())#getString ()
				in
				add {emitter=transmitter; time=t; offs=(x,y); text=str} buffer;
				browser (e#getNextBrother ())
		in let _ = browser (xmlDialog#getElementById id)
		in computing <- Some (Thread.create (Queue.iter (self#makeSurface)) buffer)

	method is_empty () =
		Queue.is_empty buffer
	method update () =
		let time = (top buffer).time in
		if time = 0 then
			(pop buffer, pop image)
		else
			let frame = (top buffer, top image) in
			(top buffer).time <- time-1;
			frame
	
	method private makeSurface frame =
		let nameChar = frame.emitter
		and text = frame.text
		in
		let (fontN, colorN) = (getCharacter nameChar)#getFont in
		let font =  fonts#get fontN
		and color = colors#get colorN
		in 
		let size = (Sdlttf.font_height font) / 10
		and txtS = Sdlttf.render_utf8_solid font text ~fg:color 
		in let (w, h, _) = surface_dims txtS
		in let (nw, nh) = (2*size+w, 2*size+h)
		and offset = rect size size 0 0
		in let outline = create_RGB_surface_format txtS [`SWSURFACE] ~w:nw ~h:nh
		in let setPoint (x,y) =
			let x = x+size
			and y = y+size
			in
			let x =
				if x < 0 then 0
				else if x > nw then nw
				else x
			and y = 
				if y < 0 then 0
				else if y > nh then nh
				else y
			in put_pixel_color outline ~x ~y black 
		and endingWork () =
			unlock txtS;
			unlock outline
		and (i, j, za, zb) = (ref 0, ref 0, ref 0, ref 0)
		in let getAlpha () =
			let (_, alpha) = get_RGBA txtS (get_pixel txtS !i !j)
			in alpha
		in let _ = 
			lock txtS;
			lock outline
		in let _ = 
			while !i < w do j := 0;
				while !j < h do
					begin
					if getAlpha () > 0 then za := 0;
						while !za < size do zb := 0;
							while !zb < size do
								setPoint (!za-(size/2), !zb-(size/2));
							zb := !zb + 1; done;
						za := !za + 1; done;
					end;
				j := !j + 1; done;
			i := !i + 1; done;
		in
		endingWork ();
		blit_surface ~src:txtS ~dst:outline ~dst_rect:offset ();
		add outline image
		
	end
;;

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
		
		method getLine (g,h) (i,j) = 
		
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
			
			and func (a,b) (x,y) =
				let a = float_of_int (a)
				and b = float_of_int (b)
				and x = float_of_int (x)
				and y = float_of_int (y)
				in
				let p = (y -. b)/.(x -. a)
				in 
				let o = (y -. p *. x)
				in
				let f (h:int) = int_of_float(p *. (float_of_int(h)) +. o)
				in
				f
				in 
				
				let f = func (g,h) (i,j)
				
				in
			
			let rec final (a,b) (x,y) =
				match (a,b) with 
				|(a,b) when (a = x)||(b = y) -> 
					line (a,b) (x,y)
				|(a,b) when x > a ->
					let c = a + 1 in
					let d = f a in
					line (a,b) (c,d);
					final (c,d) (x,y)
				|(a,b) ->
					let c = a - 1 in
					let d = f a in
					line (a,b) (c,d);
					final (c,d) (x,y)
			in
			final (g,h) (i,j)
			
		
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
					push (Animation (rect (i*w) 0 w h)) animationUpdate;
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
					if loopAnimation && not(is_empty positionUpdate) then
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
		
		val mutable currentDialog = None
		val mutable background = get_video_surface ()
		
		val displayData = new dictionary
		
		initializer
			let mask = Sdlevent.make_mask (Sdlevent.QUIT_EVENT::Sdlevent.MOUSEBUTTONUP_EVENT::Sdlevent.KEYUP_EVENT::[]) in
			set_caption (envString#get "name") (envString#get "icon");
			Sdlevent.enable_events mask;
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
			in 
			blit_surface ~src:background ~dst:(!window) ();
			browser (displayData#elements ())
		method private displayDialog = match currentDialog with
			| None -> ()
			| Some d when d#is_empty () -> currentDialog <- None
			| Some d -> 
				let (data, image) = d#update () in
				let character = data.emitter
				and offset = (data:tirade).offs
				in
				let (ox, oy) = offset
				and (cx, cy) = (displayData#get character).pos
				in let position = rect (cx+ox) (cy+oy) 0 0
				in blit_surface ~src:image ~dst:(!window) ~dst_rect:position ()
			
		
		(** Storing Data **)
		method setBackground surface =
			background <- surface
		
		method addItemToDisplay name (x,y) =
			let item = (getItem name :> displayable) in
			let animation = item#getDataAnim#getElementById "idle" in
			let w = int_of_string(animation#getAttr "w")
			and h = int_of_string(animation#getAttr "h")
			in
			let element =
				{
					data = item;
					img = rect 0 0 w h; 
					pos = (x,y); 
					updating = (new displayUpdating window item)
				}
			in
			displayData#set name element
		method addCharacterToDisplay name (x,y) =
			let character = (getCharacter name :> displayable) in
			let animation = character#getDataAnim#getElementById "idle" in
			let w = int_of_string(animation#getAttr "w")
			and h = int_of_string(animation#getAttr "h")
			in
			let element =
				{
					data = character;
					img = rect 0 0 w h;
					pos = (x,y);
					updating = (new displayUpdating window character)
				}
			in displayData#set name element
		
		method removeDisplayElement name =
			displayData#remove name
		method fushDisplayData () =
			displayData#clear ()
		
		(** Update Data **)
		method setDialog xmlDialog id =
			currentDialog <- Some (new dialog xmlDialog id)
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
		
		(** Read Input User and run the function corresponding with event **)
		method private inputUser = function 
			| _ -> ()
		
		(** Loop Displaying Event **)
		method private loop () = 
			while run do
				ticks <- 17 + Sdltimer.get_ticks (); (*17 ms -> 60fps*)
				
				self#updataDisplayData;
				self#display;
				self#displayDialog;
				flip !window;
				
				begin match Sdlevent.poll () with
					| Some Sdlevent.QUIT -> Sdl.quit (); run <- false
					| None -> ()
					| event -> self#inputUser event
				end;
				
				while (Sdltimer.get_ticks ()) <= ticks do () done;
			done
	end
;;

(* Functions ******************************************************************)

let loadFonts fontDir =
	let (dataF, dataC) = 
		try (
			let file = new Wally.treeXml (fontDir//"info.xml") in
			(file#getFirstByName "fonts", file#getFirstByName "colors")
		) with _ -> failwith ("read info.xml of fonts failed.")
	in
	let rec browserF = function 
		| e when e#is_empty () -> ()
		| e -> 
			let name = e#getAttr "name"
			and size = int_of_string (e#getAttr "size")
			and path = e#getAttr "file"
			in fonts#set name (Sdlttf.open_font path size);
			browserF (e#getNextBrother ())
	in 
	let rec browserC = function
		| e when e#is_empty () -> ()
		| e -> 
			let name = e#getAttr "name"
			and red = int_of_string(e#getAttr "r")
			and green = int_of_string(e#getAttr "g")
			and blue = int_of_string(e#getAttr "b")
			in colors#set name ((red,green,blue):color);
			browserC (e#getNextBrother ())
	in
	browserF (dataF#getChildren ());
	browserC (dataC#getChildren ())
;;


let loadImage path = 
	load_image path
;;

let initW () =
	window := Some (new sdlWindow (int_of_string(Zak.envString#get "xScreen")) (int_of_string(Zak.envString#get "yScreen")))

let getWindow () = match !window with
			|None -> raise Sdl_not_initialized
			|Some a -> a
;;

(* Debug *********************************************************************)
let test () =
	let _ = initW () in
	let w = getWindow ()
	and b = loadImage "./game/game.png" in
	w#setBackground b;
	w
;;

let lol w (x,y) =
	let _ = w#moveTo "loly" (x,y) in
	w#setAnimation "loly" "idle"
;;
