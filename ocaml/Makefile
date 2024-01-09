
all: 
	ocamlbuild main.byte 
	mv main.byte mincamlc

clean:
	rm -rf _build
	rm ../tests/gen-code/*.actual
	rm ../tests/gen-code/*.asml

cleanest: clean
	rm -f mincamlc

test:
	./../scripts/mincaml-test-parser.sh
	./../scripts/mincaml-test-typechecking.sh
	./../scripts/mincaml-test-front-end.sh
	cat ../resultats_tests.txt