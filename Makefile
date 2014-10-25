version:=$(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)
packfile=blog_core-$(version).tgz
remote=www-data@packs.rlaanemets.com:/sites/packs.rlaanemets.com/public/blog-core

test:
	swipl -s tests/tests.pl -g run_tests,halt -t 'halt(1)'

package: test admin
	tar cvzf $(packfile) prolog tests pack.pl README.md

upload: package
	scp $(packfile) $(remote)/$(packfile)

install-tools:
	$(MAKE) -C admin install-tools

admin:
	$(MAKE) -C admin all

check:
	$(MAKE) -C admin check

clean:
	$(MAKE) -C admin clean

test-admin:
	$(MAKE) -C admin test

.PHONY: test package upload admin clean check install-tools test-admin
