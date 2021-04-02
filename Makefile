build-env-file=build.env
ifdef env-file
build-env-file=$(env-file)
endif
include $(build-env-file)

WEB_IMAGE=$(REGISTRY)/$(WEB_IMAGE_NAME)

.PHONY: all
all: auth-web.image dist/doc
	$(MAKE) -C service all

.PHONY: dist/doc
dist/doc:
	$(MAKE) -C auth-service-core dist/doc
	$(MAKE) -C service dist/doc
	rm -rf dist/doc
	mkdir -p dist/doc
	cp -rf auth-service-core/dist/doc dist/doc/auth-service-core
	cp -rf service/dist/doc dist/doc/auth-service
	# pandoc Doc/API.md -o dist/doc/index.html
	cp -f Doc/API.html dist/doc/index.html

.PHONY: service/image
service/image:
	$(MAKE) -C service image

.PHONY: test
test: unittests systemtests

.PHONY: unittests
unittests:
	$(MAKE) -C service test

systemtests: export NORATELIMIT=true
systemtests: up
	tests/test dockertest
	$(MAKE) down

auth-web-deps := $(shell find web)

auth-web.image: $(auth-web-deps)
	docker build -t $(WEB_IMAGE):$(TAG) web
	echo -n "$(TAG)" > auth-web.image

.PHONY: run
run: up
	docker-compose logs --follow

dev/ed25519.priv.der:
	mkdir -p dev
	openssl genpkey -algorithm Ed25519 -outform der \
	  | base64 > dev/ed25519.priv.der

dev/ed25519.pub.der: dev/ed25519.priv.der
	base64 -d dev/ed25519.priv.der \
	  | openssl pkey -inform der -pubout -outform der \
	  | base64 > dev/ed25519.pub.der

secrets/header_signing_private_key: dev/ed25519.priv.der
	mkdir -p secrets
	mkfifo secrets/header_signing_private_key

.PHONY: up
up: service/image auth-web.image dev/ed25519.priv.der dev/ed25519.pub.der secrets/header_signing_private_key
	cat dev/ed25519.priv.der > secrets/header_signing_private_key &
	env "AUTHWEBTAG=$$(cat auth-web.image)" docker-compose up -d

.PHONY: down
down:
	docker-compose kill -s 9
	docker-compose down --remove-orphans -v

.PHONY: push
push:
	$(MAKE) -C service push
	docker push $(WEB_IMAGE):$(TAG)

.PHONY: push-latest
push-latest:
	$(MAKE) -C service push-latest
	docker tag $(WEB_IMAGE):$(TAG) $(WEB_IMAGE):latest
	docker push $(WEB_IMAGE):latest

.PHONY: clean
clean:
	$(MAKE) -C service clean
	rm -r dev
	rm -rf secrets
