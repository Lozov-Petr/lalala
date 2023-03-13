TOPFILE = WaterPuzzle
SOURCES = WaterPuzzle.ml2mk.ml

REWRITER_EXES = rewriter.native rewriter_ppx.native
PXFLAGS = -ppx './rewriter_ppx.native --as-ppx -new-typenames '
include ../Makefile.common.mk
NOCFLAGS = -pp ./rewriter.native -w -8 -rectypes
BFLAGS += -package noCanren.Peano

rewriter.native: rewriter_ppx.native
rewriter_ppx.native:
	ocamlfind opt -predicates ppx_driver -package OCanren-ppx.ppx_fresh,OCanren-ppx.ppx_distrib,GT.ppx,GT.ppx_all,ppxlib.runner -linkpkg -linkall -o $@
