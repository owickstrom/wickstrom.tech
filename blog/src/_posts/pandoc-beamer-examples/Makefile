FIRST_PDF=../../generated/pandoc-beamer-examples/first.pdf

DIAGRAM_SRCS=$(shell find diagrams -name '*.dot')
DIAGRAMS=$(DIAGRAM_SRCS:diagrams/%.dot=../../generated/pandoc-beamer-examples/%.png)

.PHONY: all
all: $(FIRST_PDF) $(DIAGRAMS)

$(FIRST_PDF): first.md
	rm -rf .tmp
	mkdir .tmp
	pandoc -t beamer -s -o .tmp/slides.tex first.md
	cd .tmp && pdflatex slides.tex
	cp .tmp/slides.pdf $(FIRST_PDF)
	rm -rf .tmp

../../generated/pandoc-beamer-examples/%.png: diagrams/%.dot
	dot -Tpng $< -o $@
