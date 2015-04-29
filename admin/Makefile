JSHINT = node_modules/.bin/jshint
BROWSERIFY = node_modules/.bin/browserify
CASPERJS = node_modules/.bin/casperjs
EXORCIST = node_modules/.bin/exorcist
PHANTOMJS = node_modules/.bin/phantomjs

BUNDLE = ../prolog/bc/public/js/admin.bundle.js
MAP = $(BUNDLE).map
BUNDLE_DEPS = admin.js lib/*.js lib/vm/*.js lib/pages/*.js lib/pages/*.html

EXTERNAL_DIR = ../prolog/bc/public/js/libs

EXTERNAL = $(EXTERNAL_DIR)/fetch.js $(EXTERNAL_DIR)/knockout.js \
	$(EXTERNAL_DIR)/es6-promise.js $(EXTERNAL_DIR)/router-standalone.js \
	$(EXTERNAL_DIR)/kontainer-standalone.js $(EXTERNAL_DIR)/speakingurl.js

export SHELLOPTS:=errexit:pipefail

.DELETE_ON_ERROR:

all: $(BUNDLE) $(EXTERNAL)

$(BUNDLE): $(BUNDLE_DEPS) Makefile
	rm -f $(MAP)
	$(BROWSERIFY) --debug -t brfs -t uglifyify $< | $(EXORCIST) $(MAP) > $@

$(EXTERNAL_DIR)/fetch.js: node_modules/whatwg-fetch/fetch.js
	cp $< $@

$(EXTERNAL_DIR)/knockout.js: node_modules/knockout/build/output/knockout-latest.js
	cp $< $@

$(EXTERNAL_DIR)/es6-promise.js: node_modules/es6-promise/dist/es6-promise.min.js
	cp $< $@

$(EXTERNAL_DIR)/router-standalone.js: node_modules/hash-regex-router/dist/router-standalone.js
	cp $< $@

$(EXTERNAL_DIR)/kontainer-standalone.js: node_modules/kontainer/dist/kontainer-standalone.js
	cp $< $@

$(EXTERNAL_DIR)/speakingurl.js: node_modules/speakingurl/speakingurl.min.js
	cp $< $@

check:
	$(JSHINT) --exclude=node_modules .

clean:
	rm -f $(BUNDLE) $(MAP) $(EXTERNAL)

install-tools:
	npm install

test:
	PHANTOMJS_EXECUTABLE=$(PHANTOMJS) $(CASPERJS) test tests

.PHONY: all check clean install-tools
