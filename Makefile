version:=$(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)
packfile=blog_core-$(version).tgz
remote=raivo@infdot.com:/files/infdot/infdot/packs/blog-core

test:
	swipl -s tests/tests.pl -g run_tests,halt -t 'halt(1)'

package: test
	tar cvzf $(packfile) prolog pack.pl README.md LICENSE

upload: package
	scp $(packfile) $(remote)/$(packfile)

.PHONY: test package upload clean check
