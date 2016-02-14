auth-service
============

An authentication micro-service written in Haskell.

Get started
-----------

The instructions below assumes that you are using Docker Compose, that the API
you want to protect is called "server", that your API is proxied to from /api,
that you are running a PostgreSQL instance called "database", and that your
PostgreSQL database and user name is "postgres".

Add this repository as a submodule, and put the following in your
docker-compose.yml configuration (assuming one instance):

    auth-service:
      build: auth-service
      links:
      - database
    auth-web:
      build: auth-service/web
      links:
      - auth-service
      - server:INSTANCE_ID

Volume ./auth-service/web/auth-service.include to
/etc/nginx/auth-service.include in your web server.

In your web nginx.conf file, put this line in your server block:

    include auth-service.include;

Also, if you want unauthenticated requests to /index.html to be translated into
the authentication page of this component, add the following as well:

    location /index.html {
        auth_request /api/auth;
        error_page 403 = /auth.html;
        expires -1;
    }

An example docker-compose.yml configuration for your web app would be:

    web:
      image: nginx:latest
      links:
      - auth-web
      volumes:
      - ./web/nginx.conf:/etc/nginx/nginx.conf:ro
      - ./web/app:/www:ro
      - ./auth-service/web/auth-service.include:/etc/nginx/auth-service.include:ro

Make sure you set the X-Instance header to the instance identifier somewhere
external to the web container. This can be done either in your client app or,
for instance, in an external router Nginx container, like so:

    location / {
        proxy_pass http://web/;
        proxy_set_header X-Instance 'INSTANCE_ID';
    }

If you want to use the authentication markup beloning to this library, make sure
to provide AngularJS and Bootstrap under /libs.js and /libs.css. Please also
provide a background image at /background.jpg. Please note that only a Swedish
version of the markup is available at this time.

Please see auth-service.config for the configuration options.

Managing users
--------------

Users can be added like so:

    docker exec -it app_auth_1 auth-service adduser "My Name" my_password my_email@example.com

Passwords can be changed like so:

    docker exec -it app_auth_1 auth-service chpass my_email@example.com my_password

Other changes can be done with SQL directly.

API
---

Authentication is done like this:

    POST /api/login
    { "user": "my_email@example.com", "password": "my_password" }

The above requests sets a cookie, "token". It also sets a header, "X-Token", in
case you would rather get the token like that. As a third option, it includes
the token in the JSON body like so:

    {"token":"the_token"}

You will need to set this token in future requests, either as a header or as a
cookie.

Logging out is done like this:

    POST /api/logout

Legal
-----

Copyright © 2015-2016 Nejla AB. All rights reserved.

Twilio and Twiml are registered trademarks of Twilio and/or its affiliates.
Other names may be trademarks of their respective owners.