.PHONY: all
.PHONY: mpi
.PHONY: clean

all: mpi
mpi:
	gcc -Wall -c mpi.c
	gcc mpi.o -o mpi -lmpi
	./mpi
	mv mpiconstants.ads ../src/distributedsystems

clean:
	$(RM) *.o
	$(RM) mpi
