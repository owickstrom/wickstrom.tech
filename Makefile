TOOLS=src/tools
PLANTUML=deps/plantuml.jar

build: $(PLANTUML)
	cd src && bundle exec jekyll build --destination ../target

serve:
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

LHS_SOURCES=$(shell find src/_lhs -name '*.lhs')
LHS_TARGETS=$(LHS_SOURCES:src/_lhs/%.lhs=src/_posts/%.md)

src/_posts/%.md: src/_lhs/%.lhs src/_lhs/%.md
	cp $(word 2,$^) $@
	echo "" >> $@
	pandoc $< -f markdown+lhs -t markdown_github --base-header-level=2 | sed 's/sourceCode/haskell/' >> $@

.PHONY: lhs
lhs: $(LHS_TARGETS)
