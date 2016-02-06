#!/bin/sh

# Copyright © 2015-2016 Nejla AB. All rights reserved.

until nc -z auth-service 80; do
    echo "Waiting for auth-service..."
    sleep 1
done

echo "Authentication web server is running!"

cd /etc/nginx && nginx -g "daemon off;"
