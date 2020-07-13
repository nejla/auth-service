build-env-file=build.env
ifdef env-file
build-env-file=$(env-file)
endif
include $(build-env-file)

TAG=$(shell git rev-parse HEAD)
SERVICE_IMAGE=$(REGISTRY)/$(APP_IMAGE_NAME)
WEB_IMAGE=$(REGISTRY)/$(WEB_IMAGE_NAME)

all: service/image auth-web-container

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


run: all
	docker-compose up

up: all
	docker-compose up -d

down:
	docker-compose down --remove-orphans -v

push:
	docker push $(WEB_IMAGE):$(TAG)
	docker push $(WEB_IMAGE):latest
	docker push $(SERVICE_IMAGE):$(TAG)
	docker push $(SERVICE_IMAGE):latest

clean:
	$(MAKE) -C service clean

.PHONY: all build run up down push service-container auth-web-container unittests systemtests test clean
