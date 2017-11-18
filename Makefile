TOOLS=src/tools
PLANTUML=deps/plantuml.jar

UML_SRCS=$(shell find src/_uml -name '*.uml.txt')
UMLS=$(UML_SRCS:src/_uml/%.uml.txt=src/generated/uml/%.svg)

build: $(PLANTUML) $(UMLS)
	make -C src/_posts/pandoc-beamer-examples all
	stack build --fast
	cd src && bundle exec jekyll build --destination ../target

serve: $(PLANTUML) $(UMLS)
	make -C src/_posts/pandoc-beamer-examples all
	stack build --fast
	cd src && bundle exec jekyll serve --destination ../target --unpublished

deploy: build
	aws s3 sync --region=eu-west-1 target s3://wickstrom.tech --acl=public-read


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
