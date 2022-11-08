ifeq ($(origin stack_args),undefined)
export stack_args := --docker --docker-stack-exe download --no-nix
endif

ifeq (${TERM},dumb)
export stack_build_args := ${stack_build_args} --colour=never
endif

build-env-file:=../build.env
ifdef env-file
build-env-file:=$(env-file)
endif
include $(build-env-file)


SERVICE_IMAGE:=$(REGISTRY)/$(APP_IMAGE_NAME)
TEST_IMAGE := $(REGISTRY)/$(APP_TEST_BASEIMAGE_NAME)

tests := dist/tests/api-tests dist/tests/unit-tests

auth-service-core:=$(shell find ../auth-service-core -type f)
srcfiles:=$(shell find src -type f)
test-srcfiles:=$(shell find test -type f)


.PHONY: all
all: image baseimage $(tests) dist/doc

.PHONY: push
push: push-image push-baseimage

.PHONY: push-latest
push-latest: push-image-latest push-baseimage-latest

image: dist/auth-service Dockerfile ../.git
	docker build --pull \
	  -t "${SERVICE_IMAGE}:${TAG}" .
	docker tag "${SERVICE_IMAGE}:${TAG}" \
	           "${SERVICE_IMAGE}:latest"
	echo "$(TAG)" > image

.PHONY: push-image
push-image: image
	docker push "$(SERVICE_IMAGE):$(shell cat image)"

.PHONY: push-image-latest
push-image-latest: image
	docker tag "$(SERVICE_IMAGE):$(shell cat image)" "$(SERVICE_IMAGE):latest"
	docker push "$(SERVICE_IMAGE):latest"


# Depend on image to ensure that baseimage is up to date with latest built image
baseimage: image
	docker build . --pull --target baseimage \
	  -t "$(TEST_IMAGE):$(TAG)"
	echo "$(TAG)" > baseimage

.PHONY: push-baseimage
push-baseimage:
	docker push $(TEST_IMAGE):$(TAG)

.PHONY: push-baseimage-latest
push-baseimage-latest:
	docker tag $(TEST_IMAGE):$(TAG) $(TEST_IMAGE):latest
	docker push $(TEST_IMAGE):latest


dist/auth-service: $(auth-service-core) $(srcfiles) $(test-srcfiles) package.yaml stack.yaml
	rm -f *.cabal
	rm -f stack.yaml.lock
	mkdir -p ./dist
	stack build --install-ghc --test --no-run-tests \
	            ${stack_args} \
	            ${stack_build_args} \
	            --haddock --no-haddock-deps --haddock-hyperlink-source \
	            --copy-bins --local-bin-path ./dist

dist/doc: dist/auth-service
	rm -rf dist/doc
	cp -fr $(shell stack path ${stack_args} --dist-dir)/doc/html/auth-service \
               dist/doc


# Tests
#######

$(tests): dist/tests/% : dist/auth-service $(test-srcfiles) package.yaml stack.yaml
	mkdir -p dist/tests
	cp "$(shell stack ${stack_args} path --dist-dir)/build/$(notdir $@)/$(notdir $@)" dist/tests/

.PHONY: tests
tests: $(tests)

.PHONY: test-down
test-down:
	docker-compose -f docker-compose.test.yaml -p auth-service-test down -v

.PHONY: test
test: $(tests)
	env BASEIMAGE=$(TEST_IMAGE):$(TAG) \
	docker-compose -f docker-compose.test.yaml -p authservice-test \
	  run --rm test

# Clean
#######

.PHONY: clean
clean:
	stack clean
	rm -f stack.yaml.lock
	rm -rf dist
	rm -f *.cabal
	rm -f image
	rm -f baseimage

.PHONY: distclean
distclean: clean
	rm -rf .stack-work
