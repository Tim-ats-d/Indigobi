.PHONY: all build clean doc repl fmt deps

file = "./src/front/handler.ml"
TARGET =

ifeq ($(TARGET),DARWIN)
	LAUNCH_APP=open
else
	ifeq ($(TARGET),UNIX)
		LAUNCH_APP=xdg-open
	endif
endif

all: build doc

build:
	$(eval TMP_BCK := $(shell mktemp))
	cp $(file) $(TMP_BCK)
	tmp=$$(mktemp); sed "s/LAUNCH_APP/$(LAUNCH_APP)/" $(file) >$$tmp; mv $$tmp $(file)
	dune build
	mv $(TMP_BCK) $(file)

doc:
	dune build @doc-private

clean:
	dune clean

repl:
	dune utop

fmt:
	dune build @fmt --auto-promote

deps:
	opam install --deps-only .

install:
	dune install

uninstall:
	dune uninstall
