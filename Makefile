CLOUDFRONT_DISTRIBUTION_ID=E372HNY0DNX3PN

all:
	@echo "Usage: make (deploy|analytics-report)"
	@exit 1

deploy:
	aws s3 sync --region=eu-west-1 $(shell nix-build blog --no-out-link) s3://wickstrom.tech --acl=public-read --profile wickstrom.tech --delete
	aws cloudfront create-invalidation --distribution-id $(CLOUDFRONT_DISTRIBUTION_ID) --paths '/*' --profile wickstrom.tech

###############################################################
# ANALYTICS 													
###############################################################

ANALYTICS_DIR=$(HOME)/opt/wickstrom-tech-analytics

.PHONY: analytics-report
analytics-report:
	@echo "Creating analytics report..."
	mkdir -p $(ANALYTICS_DIR)
	aws s3 sync s3://wickstrom-tech-access-logs/wickstrom.tech "$(ANALYTICS_DIR)"
	find $(ANALYTICS_DIR) -name '*.gz' | xargs zcat -f | grep -v feed.xml | goaccess --log-format=CLOUDFRONT --ignore-referer=wickstrom.tech --ignore-crawlers -o "$(ANALYTICS_DIR)"/report.html
	firefox "$(ANALYTICS_DIR)"/report.html
