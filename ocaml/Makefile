all: 
	ocamlbuild main.byte 
	mv main.byte mincamlc

clean:
	rm -rf _build
	rm -f ../tests/gen-code/*.actual
	rm -f ../tests/gen-code/*.asml
	rm -f ../resultats_tests.txt

cleanest: clean
	rm -f mincamlc

test: mincamlc
	@./../scripts/mincaml-test-parser.sh
	@./../scripts/mincaml-test-typechecking.sh
	@./../scripts/mincaml-test-front-end.sh
	@cat ../resultats_tests.txt