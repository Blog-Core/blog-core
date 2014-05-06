version:=$(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)
packfile=blog_core-$(version).tgz
remote=packs@packs.rlaanemets.com:/usr/share/nginx/packs.rlaanemets.com/blog-core

test:
	swipl -s tests/tests.pl -g run_tests,halt -t 'halt(1)'

package: test admin
	tar cvzf $(packfile) prolog tests pack.pl README.md

upload: package
	scp $(packfile) $(remote)/$(packfile)

admin: prolog/bc/public/admin.min.js

prolog/bc/public/admin.js: admin/*.js admin/controller/*.js admin/vm/*.js
	browserify --noparse=admin/lib/knockout.js \
		--noparse=admin/lib/q.js \
		--noparse=admin/lib/speakingurl.js \
		--outfile $@ admin/admin.js

prolog/bc/public/admin.min.js: prolog/bc/public/admin.js
	uglifyjs $< --screw-ie8 --output $@

check:
	jshint --exclude=admin/lib/* admin

clean:
	rm -f prolog/bc/public/admin.js
	rm -f prolog/bc/public/admin.min.js

.PHONY: test package upload admin clean check
