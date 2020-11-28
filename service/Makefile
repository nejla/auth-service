ifeq ($(origin stack_args),undefined)
export stack_args := --docker --docker-stack-exe download --no-nix --colour=auto
endif

ifeq (${TERM},dumb)
export stack_build_args := ${stack_build_args} --colour=never
endif

build-env-file=../build.env
ifdef env-file
build-env-file=$(env-file)
endif
include $(build-env-file)


SERVICE_IMAGE=$(REGISTRY)/$(APP_IMAGE_NAME)
TEST_IMAGE = $(REGISTRY)/$(APP_TEST_BASEIMAGE_NAME)

tests = dist/tests/api-tests dist/tests/unit-tests

srcfiles=$(shell find src -type f)
test-srcfiles=$(shell find test -type f)


.PHONY: all
all: image baseimage $(tests)

.PHONY: push
push: push-image push-baseimage

image: dist/auth-service Dockerfile ../.git
	docker build --pull \
	  -t "${SERVICE_IMAGE}:${TAG}" .
	docker tag "${SERVICE_IMAGE}:${TAG}" \
	           "${SERVICE_IMAGE}:latest"
	touch image

# Dont build, just push
.PHONY: push-image
push-image:
	docker push $(SERVICE_IMAGE):$(TAG)

# Depend on image to ensure that baseimage is up to date with latest built image
baseimage: image
	docker build . --pull --target baseimage \
	  -t "$(TEST_IMAGE):$(TAG)"
	touch baseimage

.PHONY: push-baseimage
push-baseimage:
	docker push $(TEST_IMAGE):$(TAG)


dist/auth-service: $(srcfiles) $(test-srcfiles) package.yaml stack.yaml
	rm -f *.cabal
	rm -f stack.yaml.lock
	mkdir -p ./dist
	stack build --install-ghc --test --no-run-tests \
	            ${stack_args} \
	            ${stack_build_args} \
	            --copy-bins --local-bin-path ./dist


# Tests
#######

$(tests): dist/tests/% : dist/auth-service $(test-srcfiles) package.yaml stack.yaml
	mkdir -p dist/tests
	cp "$(shell stack ${stack_args} path --dist-dir)/build/$(notdir $@)/$(notdir $@)" dist/tests/

# Bring up required services before running test suite
.PHONY: test-up
test-up: $(tests) baseimage
	docker-compose -f docker-compose.test.yaml -p auth-service-test up -d
	$(MAKE) test

.PHONY: test-down
test-down:
	docker-compose -f docker-compose.test.yaml -p auth-service-test down -v

.PHONY: test
test: $(tests)
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
