SOURCES = common.ml k_means.ml fuzzy_cmeans.ml single_pass.ml clus_algs.ml
OBJS = common.cmo k_means.cmo single_pass.cmo fuzzy_cmeans.cmo clus_algs.cmo

clus: clus_algs.ml
	ocamlfind ocamlcp -c ${SOURCES} -package extlib
	ocamlfind ocamlcp -o clus -package extlib unix.cma str.cma -linkpkg ${OBJS}
