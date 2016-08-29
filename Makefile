TOOLS=src/tools

build:
	cd src && bundle exec jekyll build --destination ../docs
	rm docs/Gemfile* docs/CNAME.old

deploy: build
	aws s3 sync docs s3://wickstrom.tech --acl=public-read


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
