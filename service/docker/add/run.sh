#!/bin/sh

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

echo "Auth-service is running!"

exec auth-service run
