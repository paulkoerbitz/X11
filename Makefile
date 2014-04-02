all:
	gcc -c idris_x11.c
	idris -o hello_x11 hello_x11.idr
