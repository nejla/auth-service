#!/bin/sh

SESSION_COOKIES=$(echo "$SESSION_COOKIES" | tr '[:upper:]' '[:lower:]')

if [ "$SESSION_COOKIES" = "true" ]; then
  PARAM_SESSION_COOKIES="-DSESSION_COOKIES=true"
elif [ -z "$SESSION_COOKIES" ] || [ "$SESSION_COOKIES" = "false" ] ; then
  PARAM_SESSION_COOKIES="-DSESSION_COOKIES=false"
else
    echo "SESSION_COOKIES variable must be one of 'true' or 'false'"
    exit 1
fi

if [ -n "$NORATELIMIT" ]; then
  PARAM_RATELIMIT="-DNORATELIMIT"
else
    PARAM_RATELIMIT="-DRATELIMIT"
fi


m4 -DAUTH_SERVICE=auth-service-backend:80 \
   "$PARAM_SESSION_COOKIES" \
   "$PARAM_RATELIMIT" \
   /nginx.conf.m4 \
   > /etc/nginx/nginx.conf

until nc -z auth-service 80; do
    echo "Waiting for auth-service..."
    sleep 1
done

echo "Authentication web server is running!"

cd /etc/nginx && nginx -g "daemon off;"
