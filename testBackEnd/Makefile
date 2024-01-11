OCAMLC=ocamlc
SOURCES=id.ml passage.ml
EXECUTABLE=passage

passage: $(EXECUTABLE)

$(EXECUTABLE): $(SOURCES)
	$(OCAMLC) -o $@ $^
	rm -f *cmi *cmo ocamlc
	./$(EXECUTABLE)

clean:
	rm -f $(EXECUTABLE) *cmi *cmo