# @IMAGE auth-service-service
# @VERSION 20200114-01

FROM ubuntu:jammy as baseimage

RUN export DEBIAN_FRONTEND=noninteractive && \
    apt-get update && \
    apt-get -y install \
      build-essential \
      curl \
      libgmp10 \
      libicu-dev \
      libpq-dev \
      libxml2-dev \
      locales \
      msmtp-mta \
      ssh-client && \
    locale-gen en_US.UTF-8

ENV LC_ALL=en_US.UTF-8
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US.UTF-8
ENV PATH=/app:$PATH

FROM baseimage

EXPOSE 80

COPY dist/auth-service /app/auth-service

CMD ["auth-service", "run"]
HEALTHCHECK CMD curl --fail "http://localhost/status"
