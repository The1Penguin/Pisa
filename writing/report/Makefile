.PHONY: pdf clean watch

pdf: build/Main.pdf

svgs = src/figure/logo.pdf
$(svgs): %.pdf : %.svg
	inkscape $< --export-type pdf

build/texmf/texmf.cnf: texmf.cnf
	mkdir -p build/texmf
	cat $< "$$(kpsewhich texmf.cnf)" > $@

build/Main.pdf: build/texmf/texmf.cnf $(svgs) $(shell find src -type f)
	cd src && \
		TEXMFCNF=../build/texmf: \
		latexmk -auxdir=../build/.aux -emulate-aux-dir -outdir=../build -xelatex -bibtex -interaction=nonstopmode -synctex=1 Main

watch:
	./watch.sh

clean:
	rm -rf build/{.aux,texmf,Main.pdf,Main.synctex*} $(svgs)
