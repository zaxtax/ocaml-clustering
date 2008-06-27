clus: clus_algs.ml
	ocamlfind ocamlcp -c clus_algs.ml -package extlib
	ocamlfind ocamlcp -o clus -package extlib unix.cma str.cma -linkpkg clus_algs.cmo
