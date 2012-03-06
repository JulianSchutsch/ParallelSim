.PHONY: all

all:
	gcc -c -o bsdsockets_constants.o ../src/network/linux/bsdsockets_constants.c
	gnatmake -Pps.gpr -largs bsdsockets_constants.o

clean:
	rm ../build/*.o
	rm ../build/*.ali
	rm bsdsockets_constants.o
