.PHONY: all build clean doc repl fmt deps

TARGET = "./src/front/handler.ml"

all: build-unix doc

build:
	dune build

build-unix:
	tmp=$$(mktemp); sed "s/LAUNCH_APP/xdg-open/" $(TARGET) >$$tmp; mv $$tmp $(TARGET)
	dune build

build-macos:
	tmp=$$(mktemp); sed "s/LAUNCH_APP/open/" $(TARGET) >$$tmp; mv $$tmp $(TARGET)
	dune build

doc:
	dune build @doc-private

clean:
	dune clean

repl:
	dune utop

fmt:
	dune build @fmt --auto-promote

deps:
	dune external-lib-deps --missing @@default

install:
	dune install

uninstall:
	dune uninstall
