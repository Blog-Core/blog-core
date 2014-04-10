version:=$(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)

test:
	swipl -s tests/tests.pl -g run_tests,halt -t 'halt(1)'

package: test admin
	tar cvzf blog-core-$(version).tgz prolog tests pack.pl README.md Makefile

upload: package
	scp blog-core-$(version).tgz packs@packs.rlaanemets.com:/usr/share/nginx/packs.rlaanemets.com/blog-core/blog-core-$(version).tgz

admin: prolog/bc/public/admin.min.js

prolog/bc/public/admin.js: admin/*.js admin/controller/*.js admin/vm/*.js
	browserify --noparse=admin/lib/knockout.js \
		--noparse=admin/lib/q.js \
		--noparse=admin/lib/speakingurl.js \
		--outfile $@ admin/admin.js

prolog/bc/public/admin.min.js: prolog/bc/public/admin.js
	uglifyjs $< --screw-ie8 --output $@

clean:
	rm -f prolog/bc/public/admin.js
	rm -f prolog/bc/public/admin.min.js

.PHONY: test package upload admin clean
