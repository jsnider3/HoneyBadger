all: HB
	@python tester.py

doc: HB
	mkdir -p docs
	ocamlfind ocamldoc defs.ml interpreter.ml -I ./ -package core -package \
		core_kernel -thread -d docs -html

HB: interpreter.cmx
	ocamlfind ocamlopt -o HB -linkpkg -package core -package core_kernel \
    -thread -w -10 parser.cmx lexer.cmx interpreter.cmx defs.cmx

interpreter.cmx: lexer.cmx
	ocamlfind ocamlopt -package core -package core_kernel -thread -w -10 \
  	-c interpreter.ml

lexer.cmx: lexer.ml parser.cmx
	ocamlfind ocamlopt    -c lexer.ml

parser.cmx: parser.mly defs.cmx
	menhir --ocamlc "ocamlfind ocamlc" --infer --base parser  parser.mly
	ocamlfind ocamlc -c parser.mli
	ocamlfind ocamlopt -c parser.ml

defs.cmx: defs.ml
	ocamlfind ocamlopt -package core -package core_kernel -thread -w -10 \
	  -c defs.ml

lexer.ml: lexer.mll
	ocamllex lexer.mll

clean:
	@rm -r docs HB *.3o ocamldoc.* *.o *.cmi *.cmx lexer.ml parser.ml parser.mli 2>/dev/null || true
