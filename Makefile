.PHONY: all
.PHONY: clean

all:
	$(MAKE) --directory=buildcfg

clean:
	$(MAKE) clean --directory=buildcfg