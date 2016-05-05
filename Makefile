build:
	jekyll build

deploy: build
	aws s3 sync _site/ s3://wickstrom.tech --acl=public-read
