#!/bin/sh

COOKIE=$(echo "$COOKIE" | tr '[:upper:]' '[:lower:]')

if [ "$COOKIE" = "session" ]; then
  PARAM_COOKIE="-DCOOKIE=session"
elif [ -z "$COOKIE" ] || [ "$COOKIE" = "permanent" ] ; then
  PARAM_COOKIE="-DCOOKIE=permanent"
else
    echo "COOKIE variable must be one of 'session' or 'permanent'"
    exit 1
fi

PARAM_NORATELIMIT=""
if [ -n "$NORATELIMIT" ]; then
  PARAM_NORATELIMIT="-DNORATELIMIT"
fi


m4 -DAUTH_SERVICE=auth-service:80 \
   "$PARAM_COOKIE" \
   "$PARAM_NORATELIMIT" \
   /nginx.conf.m4 \
   > /etc/nginx/nginx.conf

until nc -z auth-service 80; do
    echo "Waiting for auth-service..."
    sleep 1
done

echo "Authentication web server is running!"

cd /etc/nginx && nginx -g "daemon off;"
