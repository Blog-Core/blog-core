version:=$(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)

test:
	swipl -s tests/tests.pl -g run_tests,halt -t 'halt(1)'

package: test
	tar cvzf blog-core-$(version).tgz prolog tests pack.pl README.md Makefile

upload: package
	scp blog-core-$(version).tgz packs@packs.rlaanemets.com:/usr/share/nginx/packs.rlaanemets.com/blog-core/blog-core-$(version).tgz

admin: prolog/bc/public/admin.min.js

prolog/bc/public/admin.min.js: admin/*.js admin/controller/*.js
	browserify --noparse=admin/lib/knockout.js \
		--noparse=admin/lib/q.js \
		--outfile $@ admin/admin.js

.PHONY: test package upload admin
