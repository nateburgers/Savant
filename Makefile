SOURCES = $(shell ls *.ml)
TARGET = administrative_normal_form

all : $(SOURCES)
	ocamlbuild -use-ocamlfind -pkgs core,sexplib,camlp4,ollvm -tag thread $(TARGET).native

run : all
	./$(TARGET).native

clean :
	rm *.cmi *.cmx *.o
