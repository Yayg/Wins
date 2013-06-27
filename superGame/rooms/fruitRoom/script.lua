function main ()
	if get_global_int ("fruit") == 0 then
		place_item ("peche",497,327)
	end
	return 
end

function active_peche ()
	set_global_int("fruit",1)
	give_item("peche")
	remove_object ("peche")
	return
end
