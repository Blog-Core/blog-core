JSHINT = node_modules/.bin/jshint
BROWSERIFY = node_modules/.bin/browserify

BUNDLE_JS = ../prolog/bc/public/js/admin.bundle.js
MAP_JS = $(BUNDLE_JS).map

all: $(BUNDLE_JS)

$(BUNDLE_JS): admin.js *.js controller/*.js vm/*.js
	rm -f $@ $@.map
	$(BROWSERIFY) $< -p [minifyify --map admin.bundle.js.map --output $@.map] > $@

check:
	$(JSHINT) --exclude=node_modules .

clean:
	rm -f $(BUNDLE_JS) $(MAP_JS)

install-tools:
	npm install

.PHONY: all check clean install-tools
