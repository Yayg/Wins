################################################################################
#                                                                              #
#     oooooo   oooooo     oooo  o8o           A project by students EPITA      #
#      `888.    `888.     .8'   `"'                          in infosup 2017   #
#       `888.   .8888.   .8'   oooo  ooo. .oo.    .oooo.o                      #
#        `888  .8'`888. .8'    `888  `888P"Y88b  d88(  "8                      #
#  Wins   `888.8'  `888.8'      888   888   888  `"Y88b.                       #
#  is not  `888'    `888'       888   888   888  o.  )88b                      #
#  Scumm !  `8'      `8'       o888o o888o o888o 8""888P'                      # 
#                                                              Programmed by   #
#    Philémon "Phil Gekni" Gardet [philemon.gardet@epita.fr]                   #
#    Rafael "Yayg" Gozlan [rafael.gozlan@epita.fr]                             #
#    Lorry "Bardaf" Guedj [lorry.guedj@epita.fr]                               #
#    Alexandre "Stawayld" Starck [alexandre.starck@epita.fr]                   #
#                                                                              #
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

### Dependencies ###############################################################
# OCamlSDL.....(in Ubuntu depots)
# OCaml-Lua....(http://ocaml-lua.forge.ocamlcore.org/)
# OCaml-Expat..(http://mmzeeman.home.xs4all.nl/ocaml/)

### Info #######################################################################
RESULT         = wins
SOURCES        = tool.ml wally.ml zak.ml jed.ml bobbin.ml woop.ml main.ml

INCDIRS_OTHER  = -I +sdl -I +lua -I +expat
INCDIRS_UBUNTU = -I +sdl -I /usr/local/lib/ocaml/3.12.1/lua/ -I +expat

LIBSOPT    = unix.cmxa threads.cmxa bigarray.cmxa sdl.cmxa sdlloader.cmxa\
             sdlttf.cmxa sdlmixer.cmxa lua.cmxa expat.cmxa str.cmxa
LIBSTOP    = unix.cma threads.cma bigarray.cma sdl.cma sdlloader.cma sdlttf.cma\
             sdlmixer.cma lua.cma expat.cma str.cma

DOCDIR     = ./doc

OCAMLOPT   = ocamlopt.opt
OCAMLC     = ocamlc
OCAMLDOC   = ocamldoc.opt
OCAMLTOP   = ocamlmktop

D = ubuntu

### Making #####################################################################
.PHONY: doc clean

ifeq ($(D),ubuntu)
        INCDIRS = $(INCDIRS_UBUNTU)
else
        INCDIRS = $(INCDIRS_OTHER)
endif

${RESULT}: ${SOURCES}
	${OCAMLOPT} -thread $(INCDIRS) $(LIBSOPT) ${SOURCES} -o $@  

debug: mrproper 
	${OCAMLC} -thread $(INCDIRS) $(LIBSTOP) -g ${SOURCES} -o $@

top: mrproper 
	${OCAMLTOP} -thread -custom $(INCDIRS) $(LIBSTOP) ${SOURCES} -o $@
	
doc:
	${OCAMLDOC} -g ${DOCDIR} $(INCDIRS) ${SOURCES}

clean: 
	rm -rf *.cmi rm -rf *.cmx rm -rf *.o rm -rf *.cmo

mrproper: clean
	rm -rf $(RESULT) rm -rf top rm -rf debug
