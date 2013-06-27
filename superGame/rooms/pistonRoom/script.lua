function active_piston ()
	if(get_global_int("pistonUp") == 1) then
		change_room ("fruitRoom","begin")
	end
	return
end

function main ()
	place_item("piston",812,459)
	if(get_global_int("pistonUp") == 1) then
		set_animation ("piston","actived")
	end
	return 
end
