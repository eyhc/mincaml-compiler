all: 
	ocamlbuild main.byte 
	mv main.byte mincamlc

clean:
	rm -rf _build
	rm -f ../tests/gen-code/*.actual
	rm -f ../tests/gen-code/*.asml
	rm -f ../resultats_tests.txt
	rm -f ./*.asml

cleanest: clean
	rm -f mincamlc

test: all
	@./../scripts/mincaml-test-parser.sh
	@./../scripts/mincaml-test-typechecking.sh
	@./../scripts/mincaml-test-front-end.sh
	@./../scripts/mincaml-test-back-end.sh
	@cat ../resultats_tests.txt



execute : %.ml
	./../scripts/mincaml-compile.sh $^

%.ml : 
	./../scripts/mincaml-compile.sh $@