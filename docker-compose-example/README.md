# Docker Compose example

This is an example of how auth-service can be configured using Docker Compose.

## Cryptographic keys and starting the services

Start by creating two cryptographic key files needed by auth-service, and start
auth-service, the example backend, and some related services:

```
$ cd docker-compose-example
$ openssl genpkey -algorithm ed25519 -outform der | base64 > ed25519.priv.der
$ chmod 0600 ed25519.priv.der
$ base64 -d ed25519.priv.der \
    | openssl pkey -inform der -pubout -outform der \
    | base64 > ed25519.pub.der
$ docker-compose up
```

auth-service and the example backend (a total of five Docker containers) should
now be started:

```
$ docker ps
CONTAINER ID   IMAGE                            COMMAND                  CREATED          STATUS                    PORTS                                   NAMES
7aeb97d423f6   nginx                            "/docker-entrypoint.…"   2 seconds ago    Up 1 second (healthy)     0.0.0.0:8080->80/tcp, :::8080->80/tcp   docker-compose-example_proxy_1
5ca3b1c8de44   nejla/auth-service-proxy         "/docker-entrypoint.…"   4 seconds ago    Up 3 seconds (healthy)    80/tcp, 443/tcp                         docker-compose-example_auth-service-web_1
498f6922a3fd   nejla/auth-service-backend       "auth-service run"       35 seconds ago   Up 35 seconds (healthy)   80/tcp                                  docker-compose-example_auth-service-service_1
51569032e3bc   postgres                         "docker-entrypoint.s…"   38 seconds ago   Up 37 seconds (healthy)   5432/tcp                                docker-compose-example_auth-service-database_1
70f795f7afa5   docker-compose-example_backend   "docker-entrypoint.s…"   38 seconds ago   Up 37 seconds (healthy)   3000/tcp                                docker-compose-example_backend_1
```

## Add an auth-service account

In another terminal, add an auth-service account:

```
$ cd docker-compose-example
$ docker-compose exec auth-service-service auth-service adduser user@example.com secret "Example User"
$ docker-compose exec auth-service auth-service newinstance example 2451cfe5-e4db-46a9-800f-38eeec997105
$ docker-compose exec auth-service auth-service addinstance user@example.com 2451cfe5-e4db-46a9-800f-38eeec997105
```

## Accessing the example backend

Trying to access the backend without authentication won't work:

```
$ curl http://localhost:8080/api
<html>
<head><title>403 Forbidden</title></head>
<body>
<center><h1>403 Forbidden</h1></center>
<hr><center>nginx/1.19.10</center>
</body>
</html>
```

However, by signing in...

```
$ curl -H "Content-Type: application/json" -d '{ "password": "secret", "user": "user@example.com" }' http://localhost:8080/api/login
{"token":"oE8Rj8Xp96ggKmiYqOZclP","instances":[{"name":"example","id":"2451cfe5-e4db-46a9-800f-38eeec997105"}]}
```

... the backend can be accessed using a token like so:

```
$ curl -H "X-Token: oE8Rj8Xp96ggKmiYqOZclP" http://localhost:8080/api
{"message":"Hello World!"}
```

Instead of using the `X-Token` header, we can also use a cookie (with the same token value) as well:

```
$ curl -H "Cookie: token=oE8Rj8Xp96ggKmiYqOZclP" http://localhost:8080/api
{"message":"Hello World!"}
```
