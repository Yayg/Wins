open Sdlmixer

let initBobbin () =
	open_audio ()
;;

let closeBobbin () =
	close_audio ()
;;

let playMusic path =
	let file = load_music path in
	play_music file
;;


let lowSound percentage delay channel =
	let a = (int_of_float (volume_channel channel)) in
	let b = a*percentage/100 in
	setvolume_channel channel (float_of_int b);
	Sdltimer.delay(delay);
	setvolume_channel channel (float_of_int a)
;;
(*
let channelplay path =
	allocate_channels 3;
	play_channel 1 2 23  path;;
	
	*)

let gogogo () =
	allocate_channels 2
;;

let test () = 
	let son = loadWAV "appl.WAV" in
	play_channel son
;;

let playSample sound = 
	let son = loadWAV sound in
	play_channel son
;;
(*
let lowSound ?retard:(r = 0) percentage delay =
	Sdltimer.delay(r);
	let a = (int_of_float (volume_channel 0)) in
	let b = a*percentage/100 in
	setvolume_channel 0 (float_of_int b);
	Sdltimer.delay(delay);
	setvolume_channel 0 (float_of_int a)
;;*)

let lowSound ?retard:(r = 0) percentage delay =
	Sdltimer.delay(r);
	let a = (volume_channel 0) in
	let b = a*.(float_of_int percentage)/.100.0 in
	setvolume_channel 0 b;
	Sdltimer.delay(delay);
	setvolume_channel 0 a
;;

let tist () = 
	let y = 3000 in
	test();
	lowSound ~retard:y 40 1000
;;



