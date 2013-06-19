(* Jed - Display motor
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

open Queue

open Sdl
open Sdlwm
open Sdlvideo
open Sdlloader
open Sdlkey

open Zak
open Tool


(* Exceptions *****************************************************************)
exception Sdl_not_initialized
exception No_room

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
(* ************************************************************************** *)
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

(* ************************************************************************** *)
class displayUpdating window element =
	object (self)
		
		val animationUpdate = Queue.create ()
		val positionUpdate = Queue.create ()
		val direction = Queue.create ()
		
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
						| (a,b,x,y) when (x > a)&&(y > b) -> 
							push (a,b) (positionUpdate);
							push 1 direction;
							line (a + 1,b + 1) (x,y)
						| (a,b,x,y) when (x > a)&&(y < b) -> 
							push (a,b) (positionUpdate);
							push 3 direction;
							line (a + 1,b - 1) (x,y)
						| (a,b,x,y) when (x < a)&&(y < b) -> 
							push (a,b) (positionUpdate);
							push 5 direction;
							line (a - 1,b - 1) (x,y)
						| (a,b,x,y) when (x < a)&&(y > b) -> 
							push (a,b) (positionUpdate);
							push 7 direction;
							line (a - 1,b + 1) (x,y)
						(* hauteurs *)
						| (a,b,x,y) when (x = a)&&(y > b) -> 
							push (a,b) (positionUpdate);
							push 0 direction;
							line (a,b + 1) (x,y)
						| (a,b,x,y) when (x = a)&&(y < b) -> 
							push (a,b) (positionUpdate);
							push 4 direction;
							line (a,b - 1) (x,y)
						| (a,b,x,y) when (x > a)&&(y = b) -> 
							push (a,b) (positionUpdate);
							push 2 direction;
							line (a + 1,b) (x,y)
						| (a,b,x,y) when (x < a)&&(y = b) -> 
							push (a,b) (positionUpdate);
							push 6 direction;
							line (a - 1,b) (x,y)
						| _ -> ()
			
			and func (a,b) (x,y) =
				let a = float_of_int (a)
				and b = float_of_int (b)
				and x = float_of_int (x)
				and y = float_of_int (y)
				in
				let p = (y -. b)/.(x -. a) in 
				let o = (y -. p *. x) in
				let f (h:int) = int_of_float(p *. (float_of_int(h)) +. o) in
				f
			in 
			let f = func (g,h) (i,j) in
			
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
					push Nop animationUpdate;
					try ignore (pop direction)
					with _ -> ()
				done
			and animation = element#getDataAnim#getElementById name in
			let frames = (animation#getChildren ())#getElementsByName "frame" in
			let rec browser i t = function
				| [] -> ()
				| f::q -> 
					let time = int_of_string(f#getAttr "time") 
					and o = 
						try pop direction
						with _ -> 0
					in
					push (Animation (rect (i*w) (o*h) w h)) animationUpdate;
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
(* ************************************************************************** *)
class sdlWindow width height =
	object (self) 
		val window = ref (set_video_mode ~w:width ~h:height [`DOUBLEBUF])
		val mutable fullscreen = false
		val mutable run = true
		
		val modes = new dictionary
		val mutable currentMode = "game"
		val mutable currentItem = None
		
		(* Game Mode *)
		val displayData = new dictionary
		val mutable currentRoom = None
		val mutable background = get_video_surface ()
		val mutable currentDialog = None
		val mutable nodes = None
		
		(* Inventory Mode *)
		val mutable invBackground = 
			create_RGB_surface_format (get_video_surface ()) [`HWSURFACE] width height
		val mutable inventoryDisplayed = []
		
		
		initializer
			set_caption (envString#get "name") (envString#get "icon");
			modes#set "game" (self#createSurface width height);
			modes#set "inventory" (self#createSurface width height);
			modes#set "loading" (load_image ((getEnvString "dir")//"loading.png"));
			Sdlevent.disable_events Sdlevent.all_events_mask;
			Sdlevent.enable_events (Sdlevent.make_mask [Sdlevent.KEYDOWN_EVENT;Sdlevent.MOUSEBUTTONDOWN_EVENT;Sdlevent.QUIT_EVENT])
		
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
		method fushDisplayData =
			displayData#clear ()
		
		method private getRoom =
			match currentRoom with
				| Some room -> room
				| None -> raise No_room
			
		
		(** Update Game Data **)
		method setDialog xmlDialog id =
			currentDialog <- Some (new dialog xmlDialog id)
		method setAnimation objectName animationName =
			(displayData#get objectName).updating#setAnimation animationName
		method placeTo objectName newPosition =
			(displayData#get objectName).pos <- newPosition
		method moveTo objectName newPosition =
			let actualPosition = (displayData#get objectName).pos in
			(displayData#get objectName).updating#getLine actualPosition newPosition
		method changeRoom name beginNode =
			let _ =
				self#setLoadingMode;
				self#fushDisplayData
			in 
			currentRoom <- Some (getRoom name);
			ignore(self#getRoom#run "main");
			background <- load_image self#getRoom#getBackground;
			nodes <- Some self#getRoom#getNodes;
			(match nodes with
				| Some n -> n#setCurrentNode beginNode
				| None -> failwith "Initialization node during room changing is drunk !"
			); self#setGameMode
		
		(** Manager Mode **)
		method private getVideo =
		  let src = modes#get currentMode
		  and dst = !window
		  in blit_surface ~src ~dst ();
		  dst
		
		method private setGameMode =
			Sdlevent.disable_events Sdlevent.mousemotion_mask;
			currentMode <- "game"
		
		method private setInventoryMode =
			if invIsNotVoid () then begin
				Sdlevent.enable_events Sdlevent.mousemotion_mask;
				currentItem <- None;
				currentMode <- "inventory";
				self#invInitDisplay
			end
			
		method private setLoadingMode = 
			currentMode <- "loading"
		
		(** Update Game Display **)
		method private updateGame =
			self#gameUpdataDisplayData;
			self#gameDisplay;
			self#gameDisplayDialog
		 
		method private gameUpdataDisplayData =
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
		method private gameDisplay = 
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
			blit_surface ~src:background ~dst:(modes#get "game") ();
			browser (displayData#elements ())
		method private gameDisplayDialog = match currentDialog with
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
				in blit_surface ~src:image ~dst:(modes#get "game") ~dst_rect:position ()
		
		(** Update Inventory Display **)
		method private invInitDisplay =
			let (wW,hW,_) = surface_dims !window
			and ((itemsName, itemsImg), count) =
				let i = ref 0 in
				let rec browser = function
					| [] -> ([],[])
					| item::q -> 
						let itemObj = Zak.getItem item
						in let image = 
							try load_image (itemObj#getDir//itemObj#getThumnail)
							with _ -> failwith 
								("load thumnail"^(itemObj#getDir//itemObj#getThumnail)^" failed.")
						in let (nN,nI) = browser q
						in i:=!i+1;
						(item::nN,image::nI)
				in (browser (invGetItems ()), !i)
			in 
			let wL = (wW-100)/2 in
			let w =
				if count*50 < wL-10 then
					count*50
				else wL-10
			in let h = count*2500/w
			in 
			let itemSurface = self#createAlphaSurface (w+10) (h+10)
			and (xo,yo) = ((wW-w-10)/2,(hW-h-10)/2)
			in let _ =
				let nameL = Array.of_list itemsName
				and i = ref 0
				and j = ref 0
				in let rec draw iN = function
					| [] -> ()
					| img::q -> 
						let x = !i+5 and y = !j+5
						and _ =
							let pi = !i in
							i := (!i+50)mod w;
							if !i <= pi then
								j:= !j+50
						in
						set_alpha img 10;
						inventoryDisplayed <- (nameL.(iN),(x+xo,y+yo))::inventoryDisplayed;
						self#blit_alpha img itemSurface x y; 
						draw (iN+1) q
				in inventoryDisplayed <- [];
				fill_rect itemSurface (map_RGB itemSurface ~alpha:200 black);
				draw 0 itemsImg;
				blit_surface ~src:(modes#get "game") ~dst:invBackground ()
			in blit_surface ~src:itemSurface ~dst:invBackground
				~dst_rect:(rect xo yo 0 0) ()
				
		method private updateInventoryDisplay = 
			blit_surface ~src:invBackground ~dst:(modes#get "inventory") ();
			match currentItem with
				| None -> ()
				| Some name ->
					let x,y,_ = Sdlmouse.get_state ()
					and imgTxt = 
						let font = fonts#get "Inventory"
						and color = colors#get "inventory"
						in Sdlttf.render_utf8_solid font name ~fg:color 
					in blit_surface ~src:imgTxt ~dst:(modes#get "inventory") 
						~dst_rect:(rect (x+15) (y-5) 0 0) ();
					
			
			
		(** Read Input User and run the function corresponding with event **)
		(* Game Mode *)
		method private gameInputUser = function 
			| Sdlevent.KEYDOWN key ->
				begin match key.Sdlevent.keysym with
					| KEY_i -> self#setInventoryMode
					| _ -> self#updateEvents
				end
			| _ -> self#updateEvents
		
		(* Inventory Mode *)
		method private inventoryInputUser = function
		  | Sdlevent.KEYDOWN key -> 
		  	begin match key.Sdlevent.keysym with
					| KEY_i | KEY_ESCAPE -> self#setGameMode
					| _ -> self#updateEvents
				end
			| Sdlevent.MOUSEMOTION info -> 
				self#updateInvText info.Sdlevent.mme_x info.Sdlevent.mme_y
			| _ -> self#updateEvents
		
		method private updateInvText x y =
			let rec browser = function
				| [] -> 
					currentItem <- None; 
					self#updateEvents
				| (name,(i,j))::q when x>=i & x<=i+50 & y>=j & y<=j+50 ->
					currentItem <- Some name
				| _::q -> browser q
			in browser inventoryDisplayed
			
		
		(** Update Data, Display and Event **)
		method isRuning =
			run
		
		method updateWindow = 
			(* Update Data *)
			begin match currentMode with
				| "game" -> self#updateGame
				| "inventory" -> self#updateInventoryDisplay
				| "loading" -> ()
				| _ -> failwith "invalid state of programm"
			end; 
			(* Update Display *)
			flip (self#getVideo);
			(* Update Event *)
			self#updateEvents
		
		method updateEvents =
			Sdlevent.pump ();
			begin match Sdlevent.poll () with
				| Some Sdlevent.QUIT -> Sdl.quit (); run <- false
				| None -> ()
				| Some event when currentMode = "game" -> 
					self#gameInputUser event
				| Some event when currentMode = "inventory" -> 
					self#inventoryInputUser event
				| Some event when currentMode = "loading" -> 
					while Sdlevent.poll () <> None do () done
				| _ -> failwith "Error : invalid state of programm"
			end
			
		(** Low Level Functions **)
		method private displayImage clip src (x,y) = 
			let dst = self#getVideo
			and dst_rect = rect x y 0 0 in
			blit_surface ~src ~src_rect:clip ~dst ~dst_rect ()
			
		method private createAlphaSurface w h =
			let pi = surface_format !window in
			let r = pi.rmask
			and g = pi.gmask
			and b = pi.bmask
			in let a = Int32.lognot (Int32.logor r (Int32.logor g b)) 
			in create_RGB_surface [`HWSURFACE] w h pi.bits_pp r g b a
			
		method private createSurface w h =
			create_RGB_surface_format !window [`HWSURFACE] w h
			
		method blit_alpha src dst x y =
			let _ =
				lock src;
				lock dst
			and left () =
				unlock src;
				unlock dst
			and (w,h,_) = surface_dims src
			in
			for i = 0 to w-1 do
				for j = 0 to h-1 do
					let c,salpha = 
						let spix = get_pixel src i j in
						let color,alpha = get_RGBA src spix in
						(map_RGB dst ~alpha:alpha color),alpha
					in if salpha > 10 then
						put_pixel dst (x+i) (y+j) c
				done
			done;
			left ()
		
		(** Debug Method **)
		method printDisplayedElement =
			let rec mkstr = function 
				| [] -> ""
				| e::q -> e^" "^(mkstr q)
			in print_debug (mkstr (displayData#keys ()))
	end
;;

(* Functions ******************************************************************)

let loadFonts fontDir =
	let _ =
		Sdlttf.init ();
		at_exit Sdlttf.quit
	in 
	let (dataF, dataC) = 
		let file = try ((new Wally.treeXml (fontDir//"info.xml"))#getFirstByName "infoFonts" 
		) with 
			| Expat.Expat_error msg ->
				let str = Expat.xml_error_to_string msg in
				failwith ("error during opening info.xml of fonts:"^str)
			| e -> failwith ("open info.xml of fonts failed with  "^(Printexc.to_string e))
		in
		try ((file#getFirstByName "fonts", file#getFirstByName "colors")
		) with
			| Expat.Expat_error msg ->
				let str = Expat.xml_error_to_string msg in
				failwith ("error during reading info.xml of fonts:"^str)
			| e -> failwith ("read info.xml of fonts failed:"^(Printexc.to_string e))
	in
	let rec browserF = function 
		| e when e#is_empty () -> ()
		| e when (e#getXmlElement ())#getType = "Text" ->
			browserF (e#getNextBrother ())
		| e -> 
			let name = e#getAttr "name"
			and size = int_of_string (e#getAttr "size")
			and path = e#getAttr "file"
			in fonts#set name (Sdlttf.open_font (fontDir//path) size);
			browserF (e#getNextBrother ())
	in 
	let rec browserC = function
		| e when e#is_empty () -> ()
		| e when (e#getXmlElement ())#getType = "Text" ->
                        browserC (e#getNextBrother ())
		| e -> 
			let name = e#getAttr "name"
			and red = int_of_string(e#getAttr "r")
			and green = int_of_string(e#getAttr "g")
			and blue = int_of_string(e#getAttr "b")
			in colors#set name ((red,green,blue):color);
			browserC (e#getNextBrother ())
	in
	print_string "┝┅ Fonts loading...\n";
	browserF ((dataF#getChildren ())#getFirstByName "font");
	print_string "┝┅ Color fonts loading...\n";
	browserC ((dataC#getChildren ())#getFirstByName "color")
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
