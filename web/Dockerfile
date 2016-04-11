# Copyright © 2015-2016 Nejla AB. All rights reserved.

FROM phusion/baseimage

ENV NGINX_VERSION 1.9.10-1~trusty

RUN apt-key adv \
        --keyserver hkp://pgp.mit.edu:80\
        --recv-keys 573BFD6B3D8FBC641079A6ABABF5BD827BD9BF62 &&\
    echo "deb http://nginx.org/packages/mainline/ubuntu/ trusty nginx"  \
          >> /etc/apt/sources.list &&\
    apt-get update &&\
    apt-get install -y \
        dnsmasq \
        gettext-base \
        m4 \
        nginx=${NGINX_VERSION} \
        ca-certificates \
        && \
    rm -rf /var/lib/apt/lists/*

# forward request and error logs to docker log collector
RUN ln -sf /dev/stdout /var/log/nginx/access.log &&\
    ln -sf /dev/stderr /var/log/nginx/error.log

COPY dnsmasq.sh /etc/service/dnsmasq/run
COPY dnsmasq.conf /etc/dnsmasq.conf
COPY nginx.conf.m4 /nginx.conf.m4
COPY auth.html /www/auth.html
COPY auth-service.js /www/auth-service.js

RUN m4 -DAUTH_SERVICE=authservice:80 \
       /nginx.conf.m4 \
       > /etc/nginx/nginx.conf

COPY run.sh /app/run.sh

EXPOSE 80 443

CMD ["/sbin/my_init" , "--", "sh", "/app/run.sh"]
