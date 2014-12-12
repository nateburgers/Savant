SOURCES = $(shell ls *.ml)
TARGET = main

all : $(SOURCES)
	ocamlbuild -use-ocamlfind -pkgs core,sexplib,camlp4,ollvm -tag thread $(TARGET).native

run : all
	./$(TARGET).native

clean :
	rm *.cmi *.cmx *.o
