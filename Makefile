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
#    Rafaël Gozlan [rafael.gozlan@epita.fr]                                    #
#    Lorry "Bardaf" Guedj [lorry.guedj@epita.fr]                               #
#    Alexandre Starck [alexandre.starck@epita.fr]                              #
#                                                                              #
################################################################################
#    Wins is a "Point and Click" Game Motor written with OCaml                 #
#    Copyright (C) 2013    Philémon Gardet [philemon.gardet@epita.fr]          #
#                          Rafaël Gozlan [rafael.gozlan@epita.fr]              #
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

### Info #######################################################################
RESULT     = wins
SOURCES    = tool.ml wally.ml jed.ml main.ml

INCDIRS    = -I +sdl -I /usr/local/lib/ocaml/3.12.1/lua/ -I +expat
LIBS       = bigarray.cmxa sdl.cmxa sdlloader.cmxa sdlttf.cmxa sdlmixer.cmxa\
             lua.cmxa expat.cmxa

DOCDIR     = ./doc

OCAMLOPT   = ocamlopt.opt
OCAMLDOC   = ocamldoc.opt 

### Making #####################################################################
.PHONY: doc clean

${RESULT}: ${SOURCES}
	${OCAMLOPT} $(INCDIRS) $(LIBS) ${SOURCES} -o $@  

doc:
	${OCAMLDOC} -g ${DOCDIR} $(INCDIRS) ${SOURCES}

clean: 
	rm -rf *.cmi rm -rf *.cmx rm -rf *.o

