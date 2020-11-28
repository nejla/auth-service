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

## Configuring auth-service

You can configure the auth-service container by setting environment variables,
e.g. by adding `-E "option=value"` to docker calls or by setting them in your
docker-compose.yaml files

Available configuration options:

### General Options

* `TOKEN_TIMEOUT` (Int) _optional_: Time before login tokens expire in seconds after the token was created. Only affects newly created tokens. (e.g. 3600 = one hour). Tokens don't expire when unset

### Password Reset Emails
If `EMAIL_FROM` is set, all options that aren't marked as optional become
mandatory. If `EMAIL_FROM` is unset, mail is deactivated and all email-related
options are ignored

* `EMAIL_FROM` (String) _optional_: Address to send emails from
  (e.g. "myapp@example.com"). If this is unset, email is desabled and all other
  email-related options are ignored
* `EMAIL_FROM_NAME` (String) _optional_: Human-visible name to set (e.g. "My Cool App")
* `EMAIL_STMP` (String): SMTP server to send emails to (e.g. "smtp.emample.com")
* `EMAIL_PORT` (Int) _optional_: SMTP server port to connect to (default: 25)
* `EMAIL_USER` (String): SMTP username (e.g. "myapp")
* `EMAIL_PASSWORD` (String): SMTP password (e.g. "correctbatteryhorsestaple")
* `SITE_NAME` (String): Displayed name of the website in password reset emails (e.g. "This Cool App" or "thisapp.example.com")
* `RESET_LINK_EXPIRATION_TIME` (Int) _optional_: Time before password reset links expire, in hours (default: 24)
* `EMAIL_TEMPLATE` (Path) _optional_: Path to the mustache template for password reset emails (default: builtin template)
* `EMAIL_UNKNOWN_TEMPLATE` (Path) _optional_: Path to the mustache template for
  password reset emails when the email address is not in the system (default: builtin template)
* `EMAIL_LINK_TEMPLATE` (String): Template to generate links from password reset tokens, replacing %s with the token (e.g. "https://my-app.example.com/reset-password?token=%s")
* `SENDMAIL_PROGRAM` (Path) _optional_: Path of and arguments to the sendmail
  executable to use, separated by spaces (default: "/usr/sbin/sendmail")
* `EMAIL_TLS` (Bool) _optional_: Whether to use TLS to connect to the SMTP server (default: true)
* `EMAIL_AUTH` (Bool) _optional_: Whether to authenticate to the SMTP server (default: true)

* `RESET_LINK_EXPIRATION_TIME` (Int) _optional_: Time before password reset link expires, in hours (default: 24)


### Two Factor Authentication

Twilio is a mobile messaging service, used in auth-service for Two-factor authentication

* `TFA_REQUIRED` (Bool) _optional_: Whether TFA is required for all users
  (default: false). If this is set to true, Twilio account details need to be
  configures
* `TWILIO_ACCOUNT` (String) _optional_: The twilio account to use (e.g. 123456)
* `TWILIO_TOKEN` (String) _optional_: Twilio authentication token
* `TWILIO_SOURCE` (String) _optional_: The phone number to send messages from

* `OTP_LENGTH` (Int) _optional_: Number of characters to use in one time password (default: 4)
* `OTP_TIMEOUT` (Int) _optional_: Time before one time password expires, in seconds (default: 300 (five minutes))

Note that of you set _any_ of the twilio options you have to set _all_ of them.

### Account Creation

* `ACCOUNT_CREATION` (Bool) _optional_: Whether new users are allowed to create accounts (default: false)
* `DEFAULT_INSTACE` (UUID) _optional_: Default instance to set on new accounts (e.g. `6595bcf6-ba54-4054-b09a-f67618a9ba3b`)

## Configuring the auth-web container

You can set the following environment variables:

* `COOKIE` (String) _optional_: If set to `permanent` cookies are permanent, otherwise cookies are sent as session cookies

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

### Using the CLI tool
Users can be added by running the `adduser` command in the `auth-service` container:

```
docker exec -it app_auth_1 auth-service adduser "My Name" my_password my_email@example.com
```

### Using the web API

Users with the `Admin` role can create new users via the endpoint `POST
/api/users` with the following request body:

```json
{
  "uuid": "78caafc1-c71b-410a-b338-90dd7b59af0d",
  "email": "newuser@example.com",
  "password": "pasword",
  "name": "User Name",
  "phone": "123456789",
  "instances": [
    "78caafc1-c71b-410a-b338-90dd7b59af0d"
  ],
  "roles": [
    "role1"
  ]
}
```

* `uuid` and `phone` are optional (uuid will be filled in automatically if unset)

Response:

```json
{
  "user": "78caafc1-c71b-410a-b338-90dd7b59af0d",
  "roles": [ "role1" ]
}
```

### By users using the self-service API

Users may create their own accounts using the `POST /api/create-account` endpoint if
the `ACCOUNT_CREATION` environment variable is set to true. The instance of
accounts created like this is determined by the `DEFAULT_INSTANCE` variable and
the `X-Instance` header set by the auth_web container.

The request body should look like this:

```json
{
  "email": "user@example.com",
  "password": "password",
  "name": "Robert E. Xampleuser",
  "phone": "1234567"
}
```

* phone is optional

* Nothing is returned

## Changing user passwords

Login passwords can be changed by calling `/api/change-password` :

```
POST /api/change-password
  {"oldPasword":"myOldPassword","newPassword":"myNewPassword"}"

```

## Setting user roles

Users can have roles set. The roles will be passed to the backend container in
the `X-Roles` header

You can modify a users roles by using the `addrole` and `removerole` commands:

```
docker exec -it app_auth_1 auth-service addrole "my_email@example.com" myrole
```

```
docker exec -it app_auth_1 auth-service rmrole "my_email@example.com" myrole

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
