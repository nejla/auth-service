ifeq (${TERM},dumb)
export stack_build_args := ${stack_build_args} --colour=never
endif

build-env-file:=../build.env
ifdef env-file
build-env-file:=$(env-file)
endif
include $(build-env-file)


srcfiles:=$(shell find src -type f)
test-srcfiles = $(shell find test-suite -type f)

dist/lib: $(srcfiles) $(test-srcfiles) auth-service-core.cabal stack.yaml
	rm -f stack.yaml.lock
	mkdir -p ./dist
	stack build --install-ghc --test --no-run-tests \
	            ${stack_args} \
	            ${stack_build_args} \
	            --haddock --no-haddock-deps --haddock-hyperlink-source
	git rev-parse HEAD > dist/lib


dist/doc: dist/lib
	rm -rf dist/doc
	cp -fr $(shell stack path ${stack_args} --dist-dir)/doc/html/auth-service-core \
               dist/doc


# Tests
#######

tests = dist/tests/tests

$(tests): dist/tests/% : dist/lib $(srcfiles) $(test-srcfiles) auth-service-core.cabal stack.yaml
	mkdir -p dist/tests
	cp "$(shell stack ${stack_args} path --dist-dir)/build/$(notdir $@)/$(notdir $@)" dist/tests/

.PHONY: test
test: export TASTY_NUM_THREADS=1
test: $(tests)
	sh -c 'for t in dist/tests/*; do $$t; done'


# Clean
#######

.PHONY: clean
clean:
	stack clean
	rm -f stack.yaml.lock
	rm -rf dist

.PHONY: distclean
distclean: clean
	rm -rf .stack-work
