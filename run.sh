#!/bin/sh

# Copyright © 2015-2016 Nejla AB. All rights reserved.

until nc -z database 5432; do
    echo "Waiting for PostgreSQL..."
    sleep 1
done

# Prevent a possible race condition where transactions might fail since the
# database system is starting up.
sleep 5

export AUTH_SERVICE_DB_DATABASE=postgres
export AUTH_SERVICE_DB_HOST=database
export AUTH_SERVICE_DB_USER=postgres

echo "auth-service is running!"

exec auth-service run
