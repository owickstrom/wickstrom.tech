build: music-pngs
	jekyll build

deploy: build
	aws s3 sync _site/ s3://wickstrom.tech --acl=public-read


.PHONY: generate-music-symbols
generate-music-symbols:
	_tools/render-musical-symbol.sh c\'1 whole
	_tools/render-musical-symbol.sh c\'2 half
	_tools/render-musical-symbol.sh c\'4 quarter
	_tools/render-musical-symbol.sh c\'8 eighth
	_tools/render-musical-symbol.sh c\'16 sixteenth
	_tools/render-musical-symbol.sh c\'8. dotted-eighth
	_tools/render-musical-symbol.sh r1 whole-rest
	_tools/render-musical-symbol.sh r2 half-rest
	_tools/render-musical-symbol.sh r4 quarter-rest
	_tools/render-musical-symbol.sh r8 eighth-rest
	_tools/render-musical-symbol.sh r16 sixteenth-rest
	SHOW_TIME_SIGN=1 _tools/render-musical-symbol.sh '' common-time
