build: music-pngs
	jekyll build

deploy: build
	aws s3 sync _site/ s3://wickstrom.tech --acl=public-read

MUSIC_SRCS=$(shell find post-assets/music-clp-clojure -name '*.ly')
MUSIC_PNGS_1X=$(MUSIC_SRCS:%.ly=%.1x.png)
MUSIC_PNGS_2X=$(MUSIC_SRCS:%.ly=%.2x.png)

$(MUSIC_PNGS_1X): $(MUSIC_SRCS)
	cd $(shell dirname $<) \
		&& lilypond \
			--format=png \
			-dresolution=100 \
			--output $(shell basename $@ | sed 's/.png//') \
			$(shell basename $<)
	convert $@ -trim $@

$(MUSIC_PNGS_2X): $(MUSIC_SRCS)
	cd $(shell dirname $<) \
		&& lilypond \
			--format=png \
			-dresolution=200 \
			--output $(shell basename $@ | sed 's/.png//') \
			$(shell basename $<)
	convert $@ -trim $@

.PHONY: music-pngs
music-pngs: $(MUSIC_PNGS_1X) $(MUSIC_PNGS_2X)
