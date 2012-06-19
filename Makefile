.PHONY: all
.PHONY: clean

all: client

client:
	$(MAKE) client -C buildcfg

server:
	$(MAKE) server -C buildcfg

clean:
	$(MAKE) clean -C buildcfg
