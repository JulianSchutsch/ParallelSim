.PHONY: all
.PHONY: mpich2
.PHONY: clean

all:

mpich2:
	gcc -Wall -c mpi.c `pkg-config --cflags mpich2-ch3`
	gcc mpi.o -o mpi -lmpi
	./mpi
	mv mpiconstants.ads ../src/distributedsystems

clean:
	$(RM) *.o
	$(RM) mpi
