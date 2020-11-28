build-env-file=build.env
ifdef env-file
build-env-file=$(env-file)
endif
include $(build-env-file)

WEB_IMAGE=$(REGISTRY)/$(WEB_IMAGE_NAME)

all: auth-web-container
	$(MAKE) -C service all

.PHONY: service/image
service/image:
	$(MAKE) -C service image

test: unittests systemtests

unittests:
	$(MAKE) -C service test

systemtests: export NORATELIMIT=true
systemtests:
	docker-compose up -d
	tests/test dockertest
	make down

auth-web-container:
	docker build -t $(WEB_IMAGE) web
	docker tag $(WEB_IMAGE):latest $(WEB_IMAGE):latest
	docker tag $(WEB_IMAGE):latest $(WEB_IMAGE):$(TAG)


run: service/image auth-web-container
	docker-compose up

up: service/image auth-web-container
	docker-compose up -d

down:
	docker-compose down --remove-orphans -v

push:
	$(MAKE) -C service push
	docker push $(WEB_IMAGE):$(TAG)
	docker push $(WEB_IMAGE):latest

clean:
	$(MAKE) -C service clean

.PHONY: all build run up down push service-container auth-web-container unittests systemtests test clean
