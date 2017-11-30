# @IMAGE auth-service
# @VERSION 20170529-1

FROM debian:jessie
MAINTAINER Nejla AB

EXPOSE 3000

RUN echo "dependencies v1" && \
    export DEBIAN_FRONTEND=noninteractive && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442 && \
    echo 'deb http://download.fpcomplete.com/debian jessie main' > /etc/apt/sources.list.d/fpco.list && \
    apt-get update && \
    apt-get -y install \
      g++ \
      libicu-dev \
      libpq-dev \
      libstdc++-4.8-dev \
      netcat \
      postgresql-client \
      stack \
        && \
    ln -s /usr/lib/x86_64-linux-gnu/libstdc++.so.6 /usr/lib/libstdc++.so && \
    rm -rf /var/lib/apt/lists/*

ENV STACK_RESOLVER=lts-8.15
ENV GITLAB_HOST_KEY="git.nejla.com,88.99.82.139 ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBPWj+vqKUC95tAplBsYhbJkd7j0/DKDrUriDZLD+OdH3rT31+GQmaCk+TSn0RwlCxPoK14+kRunwMHY1LYdeDoY="

RUN stack setup --resolver=$STACK_RESOLVER

COPY service/auth-service.cabal /opt/auth-service-cabal/auth-service.cabal
COPY service/stack.yaml /opt/auth-service-cabal/stack.yaml
COPY auth-service-types /opt/auth-service-types
COPY buildkey /buildkey

RUN eval $(ssh-agent) && \
    chmod 600 /buildkey && \
    ssh-add /buildkey && \
    mkdir -p ~/.ssh && \
    echo "$GITLAB_HOST_KEY" >> ~/.ssh/known_hosts && \
    cd /opt/auth-service-cabal && \
    stack setup --resolver=$STACK_RESOLVER && \
    stack install --resolver=$STACK_RESOLVER --dependencies-only && \
    rm -R /opt/auth-service-cabal

COPY service /opt/auth-service
RUN cd /opt/auth-service && \
    stack install --resolver=$STACK_RESOLVER && \
    rm -R /opt/auth-service

ENV PATH /root/.local/bin:$PATH

COPY docker/add/run.sh /run.sh

CMD ["sh", "/run.sh"]
