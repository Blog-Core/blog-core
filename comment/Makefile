JSHINT = node_modules/.bin/jshint
BROWSERIFY = node_modules/.bin/browserify
CASPERJS = node_modules/.bin/casperjs
EXORCIST = node_modules/.bin/exorcist
CASPERJS = ./node_modules/.bin/casperjs --ssl-protocol=tlsv1

BUNDLE = ../public/comment.js
MAP = $(BUNDLE).map

export SHELLOPTS:=errexit:pipefail

.DELETE_ON_ERROR:

all: $(BUNDLE)

$(BUNDLE): comment.js Makefile
	rm -f $(MAP)
	$(BROWSERIFY) --standalone bcComment --debug -t brfs -t uglifyify $< | $(EXORCIST) $(MAP) > $@

test:
	$(CASPERJS) test tests/stories/validation.js

.PHONY: all
