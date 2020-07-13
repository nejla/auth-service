TAG=$(shell git rev-parse HEAD)
SERVICE_IMAGE=$(REGISTRY)/$(APP_IMAGE_NAME)

image: build Dockerfile
	docker build -t "registry.nejla.com/nejla-ab/auth-service/auth-service:${TAG}" .
	docker tag "registry.nejla.com/nejla-ab/auth-service/auth-service:${TAG}" \
	           "registry.nejla.com/nejla-ab/auth-service/auth-service:latest"
	touch image

.PHONY: build
build: dist/auth-service
	mkdir -p dist
	stack build --install-ghc ${stack_build_args} --test --no-run-tests


.PHONY: test
test: dist/auth-service
	stack test ${stack_build_args} --install-ghc

dist/auth-service: src/* src/**/* package.yaml stack.yaml
	rm -f *.cabal
	rm -f stack.yaml.lock
	mkdir -p ./dist
	stack build --install-ghc --test --no-run-tests ${stack_build_args} \
	      --copy-bins --local-bin-path ./dist

.PHONY: clean
clean:
	stack clean
	rm -f stack.yaml.lock
	rm -rf dist
	rm -f *.cabal
	rm -f image

.PHONY: distclean
distclean: clean
	rm -rf .stack-work
