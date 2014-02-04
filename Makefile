version:=$(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)

test:
	swipl -s tests/tests.pl -g run_tests,halt -t 'halt(1)' -- --port=18008 --fork=false

package: test
	tar cvzf blog-core-$(version).tgz prolog tests pack.pl README.md Makefile

upload: package
	scp blog-core-$(version).tgz packs@packs.rlaanemets.com:/usr/share/nginx/packs.rlaanemets.com/blog-core/blog-core-$(version).tgz

.PHONY: test package upload
