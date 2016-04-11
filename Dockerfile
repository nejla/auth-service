# Copyright © 2015-2016 Nejla AB. All rights reserved.

FROM haskell:7.10
MAINTAINER NejlaAB

EXPOSE 80

RUN echo "dependencies v1" && \
    DEBIAN_FRONTEND=noninteractive apt-get update && \
    apt-get -y install \
      g++ \
      libicu-dev \
      libpq-dev \
      libstdc++-4.8-dev \
      netcat \
      postgresql-client \
        && \
    ln -s /usr/lib/x86_64-linux-gnu/libstdc++.so.6 /usr/lib/libstdc++.so && \
    rm -rf /var/lib/apt/lists/*

COPY auth-service.cabal /opt/auth-service-cabal/auth-service.cabal
RUN cd /opt/auth-service-cabal && \
    cabal update && \
    cabal --ignore-sandbox install --only-dep . -j4 --force-reinstall && \
    rm -R /opt/auth-service-cabal

COPY . /opt/auth-service
RUN cd /opt/auth-service && \
    cabal --ignore-sandbox install . --force-reinstall && \
    rm -R /opt/auth-service

ENV PATH /root/.cabal/bin:$PATH

COPY run.sh /run.sh

CMD ["sh", "/run.sh"]
