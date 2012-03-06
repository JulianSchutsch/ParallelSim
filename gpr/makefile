.PHONY: all

all:
	gcc -c -o bsdsockets_constants.o ../src/network/linux/bsdsockets_constants.c
	gnatmake -c -Pps.gpr
	gnatmake -b -Pps.gpr
	gnatmake -l -Pps.gpr -largs bsdsockets_constants.o
