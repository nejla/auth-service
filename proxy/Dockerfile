FROM nginx:1.25


RUN apt-get update &&\
    apt-get install -y \
        ca-certificates \
        curl \
        gettext-base \
        netcat-openbsd \
        && \
    rm -rf /var/lib/apt/lists/* &&\
    curl -L -o mustache.tar.gz "https://github.com/cbroglie/mustache/releases/download/v1.4.0/mustache_1.4.0_linux_amd64.tar.gz" &&\
    echo "5f3a9722a071bb9e2aa16d7d575881ff93223e0103059afae6d52c01d15eb96a  mustache.tar.gz" | sha256sum -c - &&\
    tar -xf mustache.tar.gz -- mustache &&\
    install -D mustache /opt/mustache &&\
    rm -f mustache.tar.gz mustache



# forward request and error logs to docker log collector
RUN ln -sf /dev/stdout /var/log/nginx/access.log &&\
    ln -sf /dev/stderr /var/log/nginx/error.log

COPY nginx.conf.mustache /nginx.conf.mustache
COPY auth.html /www/authentication/index.html
COPY auth-service.js /www/auth-service.js
COPY run.sh /app/run.sh

EXPOSE 80 443

CMD ["bash", "/app/run.sh"]
HEALTHCHECK CMD service nginx status || exit 1
