FIRST_PDF=../../generated/pandoc-beamer-examples/first.pdf

.PHONY: all
all: $(FIRST_PDF)

$(FIRST_PDF): first.md
	rm -rf .tmp
	mkdir .tmp
	pandoc -t beamer -s -o .tmp/slides.tex first.md
	cd .tmp && pdflatex slides.tex
	cp .tmp/slides.pdf $(FIRST_PDF)
	rm -rf .tmp
