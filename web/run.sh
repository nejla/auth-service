#!/bin/sh

COOKIE=$(echo "$COOKIE" | tr '[:upper:]' '[:lower:]')

if [ "$COOKIE" = "session" ]; then
  m4 -DAUTH_SERVICE=auth-service:80 \
     -DCOOKIE=session \
     /nginx.conf.m4 \
     > /etc/nginx/nginx.conf
else if [ -z $COOKIE ] || [ "$COOKIE" = "permanent" ] ; then
   m4 -DAUTH_SERVICE=auth-service:80 \
      -DCOOKIE=permanent \
     /nginx.conf.m4 \
     > /etc/nginx/nginx.conf
     else
         echo "COOKIE variable must be one of 'session' or 'permanent'"
         exit 1
     fi
fi

until nc -z auth-service 80; do
    echo "Waiting for auth-service..."
    sleep 1
done

echo "Authentication web server is running!"

cd /etc/nginx && nginx -g "daemon off;"
