.PHONY: all
.PHONY: clean
.PHONY: configprog

client: configprog
	./configprog default \mpi

server: configprog
	./configprog default

configprog:
	gprbuild configprog.gpr

clean:
	gprclean configprog.gpr
