CLOUDFRONT_DISTRIBUTION_ID=E372HNY0DNX3PN
TOOLS=src/tools
PLANTUML=deps/plantuml.jar

UML_SRCS=$(shell find src/_uml -name '*.uml.txt')
UMLS=$(UML_SRCS:src/_uml/%.uml.txt=src/generated/uml/%.svg)

DRAFT_SRCS=$(shell find src/_drafts -name '*.md')
DRAFTS_PDF=$(DRAFT_SRCS:src/_drafts/%.md=target/drafts/%.pdf)
DRAFTS_HTML=$(DRAFT_SRCS:src/_drafts/%.md=target/drafts/%.html)

PANDOC_DRAFT_OPTS = -V date:'$(shell date --iso-8601) (draft)'
PANDOC_DRAFT_PDF_OPTS = -V header-includes:'\usepackage[T1]{fontenc}' \
												-V header-includes:'\usepackage[lf]{Baskervaldx}' \
												-V header-includes:'\usepackage{inconsolata}' \
												-V urlcolor:blue \
												-V geometry:paperwidth=4in \
												-V geometry:paperheight=8in \
												-V geometry:margin=.25in

build: $(PLANTUML) $(UMLS)
	make -C src/_posts/pandoc-beamer-examples all
	cabal new-build
	cd src && bundle exec jekyll build --destination ../target/html

serve: $(PLANTUML) $(UMLS)
	make -C src/_posts/pandoc-beamer-examples all
	cabal new-build
	cd src && bundle exec jekyll serve --drafts --destination ../target/html --unpublished

target/drafts/%.pdf: src/_drafts/%.md
	mkdir -p $(shell dirname $@)
	pandoc --resource-path=.:src $(PANDOC_DRAFT_OPTS) $(PANDOC_DRAFT_PDF_OPTS) $< -o $@

target/drafts/%.html: src/_drafts/%.md
	mkdir -p $(shell dirname $@)
	pandoc -s --resource-path=.:src $(PANDOC_DRAFT_OPTS) $< -o $@

drafts: $(DRAFTS_PDF) $(DRAFTS_HTML)

clean:
	rm -rf target

deploy: build
	aws s3 sync --region=eu-west-1 target/html s3://wickstrom.tech --acl=public-read
	aws cloudfront create-invalidation --distribution-id $(CLOUDFRONT_DISTRIBUTION_ID) --paths '/*'

.PHONY: generate-music-symbols
generate-music-symbols:
	$(TOOLS)/render-musical-symbol.sh c\'1 whole
	$(TOOLS)/render-musical-symbol.sh c\'2 half
	$(TOOLS)/render-musical-symbol.sh c\'4 quarter
	$(TOOLS)/render-musical-symbol.sh c\'8 eighth
	$(TOOLS)/render-musical-symbol.sh c\'16 sixteenth
	$(TOOLS)/render-musical-symbol.sh c\'8. dotted-eighth
	$(TOOLS)/render-musical-symbol.sh r1 whole-rest
	$(TOOLS)/render-musical-symbol.sh r2 half-rest
	$(TOOLS)/render-musical-symbol.sh r4 quarter-rest
	$(TOOLS)/render-musical-symbol.sh r8 eighth-rest
	$(TOOLS)/render-musical-symbol.sh r16 sixteenth-rest
	$(TOOLS)/render-musical-symbol.sh '\key g \major' sharp
	$(TOOLS)/render-musical-symbol.sh '\key d \minor' flat
	SHOW_TIME_SIGN=1 $(TOOLS)/render-musical-symbol.sh '' common-time

$(PLANTUML):
	mkdir -p deps
	wget http://sourceforge.net/projects/plantuml/files/plantuml.jar/download -O $@

src/generated/uml/%.svg: src/_uml/%.uml.txt src/_uml/styles.iuml $(PLANTUML)
	mkdir -p $(shell dirname $@)
	cat $< | java -jar $(PLANTUML) -tsvg -pipe > $@
