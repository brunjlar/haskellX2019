.DEFAULT: all

all: slides.pdf

slides.pdf: slides.tex stack.yaml code/exchange2019.cabal $(shell find code/src -type f)
	stack build
	latexmk -pdf -outdir=output slides.tex
	ln -sf output/slides.pdf slides.pdf

clean:
	rm -f *.pdf
	rm -rf output
