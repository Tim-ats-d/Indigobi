.PHONY: all build clean doc repl fmt deps

TARGET = "./src/front/handler.ml"

all: build-unix doc

build:
	dune build

build-unix:
	$(eval TMP_BCK := $(shell mktemp))
	cat $(TARGET) > $(TMP_BCK)
	tmp=$$(mktemp); sed "s/LAUNCH_APP/xdg-open/" $(TARGET) >$$tmp; mv $$tmp $(TARGET)
	dune build
	mv $(TMP_BCK) $(TARGET)

build-macos:
	$(eval TMP_BCK := $(shell mktemp))
	cat $(TARGET) > $(TMP_BCK)
	tmp=$$(mktemp); sed "s/LAUNCH_APP/open/" $(TARGET) >$$tmp; mv $$tmp $(TARGET)
	dune build
	mv $(TMP_BCK) $(TARGET)

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
