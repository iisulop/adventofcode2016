
adventofcode9: adventofcode9.ml adventofcode9.mli
	ocamlopt -o $@ str.cmxa adventofcode9.ml

adventofcode9.mli:
	ocamlc -i $< > $@
	ocamlc -c $@

adventofcode7: adventofcode7.go
	go build adventofcode7.go
