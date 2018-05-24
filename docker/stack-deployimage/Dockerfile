# @IMAGE auth-service-baseimage
# @VERSION 20180524-01

From ubuntu:xenial

RUN export DEBIAN_FRONTEND=noninteractive && \
    apt-get update && \
    apt-get -y install \
      build-essential \
      libgmp10 \
      libicu-dev \
      libpq-dev \
      locales \
      netcat \
      msmtp-mta \
      ssh-client && \
    locale-gen en_US.UTF-8

ENV LC_ALL=en_US.UTF-8
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US.UTF-8

EXPOSE 80

CMD /app/run.sh
