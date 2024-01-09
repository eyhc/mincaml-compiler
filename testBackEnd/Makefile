OCAMLC=ocamlc
SOURCES=id.ml passage.ml
EXECUTABLE=mincamlc

passage: $(EXECUTABLE)

$(EXECUTABLE): $(SOURCES)
	$(OCAMLC) -o $@ $^

clean:
	rm -f $(EXECUTABLE) *cmi *cmo ocamlc
