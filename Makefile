SOURCES = common.ml k_means.ml fuzzy_cmeans.ml suffix_clus.ml single_pass.ml clus_algs.ml
OBJS = common.cmo k_means.cmo single_pass.cmo fuzzy_cmeans.cmo cis.cmo lSet.cmo suffix_tree.cmo suffix_clus.cmo clus_algs.cmo

clus: ${SOURCES}
	ocamlfind ocamlcp -c ${SOURCES} -package extlib
	ocamlfind ocamlcp -o clus -package extlib unix.cma str.cma -linkpkg ${OBJS}
clean: 
	rm *.cmo *.cmi
