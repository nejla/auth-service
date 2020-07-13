FROM nginx


RUN apt-get update &&\
    apt-get install -y \
        ca-certificates \
        curl \
        dnsmasq \
        gettext-base \
        m4 \
        netcat \
        && \
    rm -rf /var/lib/apt/lists/*

# forward request and error logs to docker log collector
RUN ln -sf /dev/stdout /var/log/nginx/access.log &&\
    ln -sf /dev/stderr /var/log/nginx/error.log

COPY dnsmasq.sh /etc/service/dnsmasq/run
COPY dnsmasq.conf /etc/dnsmasq.conf
COPY nginx.conf.m4 /nginx.conf.m4
COPY auth.html /www/authentication/index.html
COPY auth-service.js /www/auth-service.js
COPY run.sh /app/run.sh

EXPOSE 80 443

CMD ["sh", "/app/run.sh"]
HEALTHCHECK CMD service nginx status || exit 1
