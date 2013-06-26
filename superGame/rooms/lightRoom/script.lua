function active_lampe ()
	if(get_global_int ("lampe") == 0) then
		set_animation ("lampe","actived")
		set_global_int ("lampe",1)
		set_global_int ("guy1s",1)
	else
		set_animation ("lampe","idle")
		set_global_int ("lampe",0)
	end
	return
end

function main ()
	place_item("lampe",560,500)
	if(get_global_int ("lampe") == 1) then
		set_animation ("lampe","idle")
		set_animation ("lampe","actived")
	end
	return 
end
