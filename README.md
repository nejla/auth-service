# auth-service

An authentication micro-service written in Haskell.

# Getting started

## Container Structure

auth-service can be used to secure a single application instance or multiple
instance with single-sign-on (SSO).

An auth-service enabled system has the following containers:
* __Application backend__: Webserver(s) of you application that
  servers the actual content. (Most likely one per application)
* __frontend__: Your frontend proxy sitting in front of auth-web. It is the
  entry point for you web users, sets the instance ID
* __auth-web__: An nginx-proxy that sits in front of your application and
  ensures that all requests are authorized. Passes authorization requests to
  __auth-service__ (One)
* __auth-service__: The authentication backend. Handles user management,
  authorization requests etc.
* __database__: Storage for __auth-service__ (One)

Here's a diagram to visualize the dependency / request flow graph:


```
    Interwebs
        |
        |
        V
 +----------------+
 +frontend (proxy)+ ...
 +----------------+
        |
        |
        V
 +----------------+    +-----------------------+
 +auth-web (proxy)+--->+Your application server| ...
 +----------------+    +-----------------------+
        |
        |
        V
 +------------+    +--------+
 |auth-service+--->+database|
 +------------+    +--------+
```

_NB_: _all_ frontend containers talk to the same auth-web container, which in
turns talks to _all_ the application containers

The instructions below make the following assumptions:
* You are using docker compose
* The API you want to protect is in a container called `server`
* Your proxy redirects endpoints under `/api` to `auth-service`
* You are running a PostgreSQL instance called `database`
* Your PostgreSQL database and user name are `postgres`

Add this repository as a *git submodule*, and put the following in your
docker-compose.yml configuration:

```
    auth-serviceb:
      build: auth-service
      links:
      - database
    auth-web:
      build: auth-service/web
      links:
      - auth-service
      - server:INSTANCE_ID
```

You can also set configuration options as environment variables here. See
auth-service.config for availableoptions.

Example:

```
      environment:
        - log=true
        - TWILIO_ACCOUNT=<account>
        - TWILIO_TOKEN=<token>
        - TWILIO_SOURCE=<twilio source phone number>
```

## Configuring the frontend proxy container

To facilitate the use of auth-web service, you need to include
auth-service.include from the frontend directory. To do this:

* volume bind `./auth-service/frontend/auth-service.include` to
`/etc/nginx/auth-service.include` in your web server by adding

```
    volumes:
      - ./frontend/auth-service.include:/etc/nginx/auth-service.include
```
to the frontend service section in your docker-compose.yml

* Add the following to your nginx.conf

```
    include auth-service.include;
```


Also, if you want unauthenticated requests to /index.html to be translated into
the authentication page of this component, add the following:

```
    location /index.html {
        auth_request /api/auth;
        error_page 403 =303 /auth.html;
    }
```


Example docker-compose.yml configuration for your frontend container:

```
    web:
      image: nginx:latest
      links:
      - auth-web
      volumes:
      - ./web/nginx.conf:/etc/nginx/nginx.conf:ro
      - ./web/app:/www:ro
      - ./auth-service/frontend/auth-service.include:/etc/nginx/auth-service.include:ro
```

Make sure you set the `X-Instance` header to the instance identifier somewhere
external to the web container. This can be done either in your client app or,
for instance, in the fronntend Nginx container, like so:

```
    location / {
        proxy_pass http://web/;
        proxy_set_header X-Instance 'INSTANCE_ID';
    }
```

If you want to use the authentication markup beloning to this library, make sure
to provide :

* AngularJS and Bootstrap under /libs.js and /libs.css.
* A background image at /background.jpg.
Please note that only a Swedish version of the markup is available at this time.

Please see auth-service.config for the configuration options.

# Managing users

## Creating new users

Users can be added by running the `adduser` command in the `auth-service` container:

```
docker exec -it app_auth_1 auth-service adduser "My Name" my_password my_email@example.com
```

## Changing user passwords

Login passwords can be changed by calling `/api/change-password` :

```
POST /api/change-password
  {"oldPasword":"myOldPassword","newPassword":"myNewPassword"}"

```

Other changes can be done with SQL directly.

# Authentication

## Logging in

Authentication is done via the `/api/login` endpoint.

### Logging in without a One Time Password (OTP):

To log in without an OTP (either because it's not known yet or not required):

```
POST /api/login
  { "user": "my_email@example.com", "password": "my_password" }
```

On success, the call will return a token (see "session token" section) if no OTP
is required.  Otherwise it will return an error:

```
'{"error":"One time password required"}'
```
and the OTP will be sent out to the user

### Logging in with an OTP

To log in with an OTP:

```
POST /api/login
  { "user": "my_email@example.com", "password": "my_password", , "otp":"myOTP" }
```

on success, a session token will be returned (see next section)

### Session Token

On successful login, a session token is returned in the following ways:

* As a _header_: `X-Token`
* As a _cookie_: `token`
* As a _response body_ : `{"token":"the_token"}`

## Accessing resources

You need to send the roken recevied from `login` in all requests, either as a
`X-Token` header or as a `token` cookie.

## Logging out
Logging out is done like this:

```
POST /api/logout
```

It deactivates the current token. Doesn't return anything

## Deactivating all _other_ tokens

You can deactivate all tokens belonging to the current user _except_ the current
one by calling

```
POST /api/disable-sessions
```

Legal
-----

Copyright © 2015-2017 Nejla AB. All rights reserved.

Twilio and Twiml are registered trademarks of Twilio and/or its affiliates.
Other names may be trademarks of their respective owners.
