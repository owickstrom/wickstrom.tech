# a makefile which builds a static html blog from the sources in `src/`
# and puts the result in `target/`

###############################################################
# BUILD
###############################################################

# the directory where the blog sources are
SRC_DIR = src
# the directory where the blog will be built
TARGET_DIR = target

SOURCES=$(shell find $(SRC_DIR)/posts -type f -name '*.md' | sort --reverse)
TARGETS=$(patsubst $(SRC_DIR)/posts/%.md,$(TARGET_DIR)/%.html,$(SOURCES))

ASSET_SOURCES=$(shell find $(SRC_DIR)/assets -type f)
ASSET_TARGETS=$(patsubst $(SRC_DIR)/%,$(TARGET_DIR)/%,$(ASSET_SOURCES))

REDIRECT_SOURCES=$(shell find $(SRC_DIR)/redirects -type f)
REDIRECT_TARGETS=$(patsubst $(SRC_DIR)/redirects/%,$(TARGET_DIR)/%,$(REDIRECT_SOURCES))

all: $(TARGETS) $(ASSET_TARGETS) $(REDIRECT_TARGETS) $(TARGET_DIR)/reset.css $(TARGET_DIR)/index.css $(TARGET_DIR)/index.html $(TARGET_DIR)/feed.xml Makefile

$(TARGET_DIR)/%.html: $(SRC_DIR)/posts/%.md src/head.html src/post-nav-before.html src/post-nav-after.html
	mkdir -p $(dir $@)
	pandoc -s -f markdown -t html5 --mathjax --highlight-style=monochrome --css=reset.css --css=index.css --include-in-header=src/head.html --include-before-body=src/post-nav-before.html --include-after-body=src/post-nav-after.html -o $@ $<

$(TARGET_DIR)/index.html: $(SOURCES) build-index.sh
	mkdir -p $(dir $@)
	./build-index.sh $(SOURCES) > $@

$(TARGET_DIR)/feed.xml: $(SOURCES) build-feed.sh
	mkdir -p $(dir $@)
	./build-feed.sh $(SOURCES) > $@

$(TARGET_DIR)/%.css: src/%.css
	mkdir -p $(dir $@)
	cp $< $@

$(TARGET_DIR)/assets/%: src/assets/%
	mkdir -p $(dir $@)
	cp $< $@

$(TARGET_DIR)/%: src/redirects/%
	mkdir -p $(dir $@)
	cp $< $@

clean:
	rm -rf $(TARGET_DIR)

###############################################################
# DEPLOYMENT
###############################################################

CLOUDFRONT_DISTRIBUTION_ID=E372HNY0DNX3PN

deploy: all
	aws s3 sync --region=eu-west-1 $(TARGET_DIR) s3://wickstrom.tech --acl=public-read --profile wickstrom.tech --delete
	aws cloudfront create-invalidation --distribution-id $(CLOUDFRONT_DISTRIBUTION_ID) --paths '/*' --profile wickstrom.tech

###############################################################
# ANALYTICS
###############################################################

ANALYTICS_DIR=$(HOME)/.cache/wickstrom-tech-analytics

.PHONY: analytics-report
analytics-report:
	@echo "Creating analytics report..."
	mkdir -p $(ANALYTICS_DIR)
	aws s3 sync s3://wickstrom-tech-access-logs/wickstrom.tech "$(ANALYTICS_DIR)"
	find $(ANALYTICS_DIR) -name '*.gz' | xargs zcat -f | grep -v feed.xml | goaccess --log-format=CLOUDFRONT --ignore-referrer=wickstrom.tech --ignore-crawlers -o "$(ANALYTICS_DIR)"/report.html
	open "$(ANALYTICS_DIR)"/report.html
