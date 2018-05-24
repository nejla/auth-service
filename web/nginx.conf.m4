# use m4 to configure
#
# Example:
# m4 -DAUTH_SERVICE=localhost:3000 \
#    -DUPSTREAM_PORT=4000 \
#    -DACCESS_LOG=/var/log/nginx/access.log
#    -DERROR_LOG=/var/log/nginx/error.log
#    -DPORT=80 \
#    nginx.conf.m4 \
#    > nginx.conf

worker_processes 1;

ifdef(`ERROR_LOG', `error_log ERROR_LOG warn;')

events {
    worker_connections 1024;
}


http {
    ifdef(`NORATELIMIT', `', `
    limit_req_zone $binary_remote_addr zone=login:10m rate=2r/m;
    limit_req_zone $binary_remote_addr zone=service:10m rate=5r/s;
    limit_req zone=service burst=10;
    limit_req_status 429; # Too Many Requests
    ')
    # Make sure we see the real address so we can rate limit according to it
    set_real_ip_from 0.0.0.0/0;
    real_ip_header  X-Forwarded-For;


    include /etc/nginx/mime.types;
    default_type application/octet-stream;
    sendfile on;
    keepalive_timeout 65;
    ifdef(`ACCESS_LOG', `access_log ACCESS_LOG;')

    server {
        ifdef(`PORT',`listen PORT;',`listen 80;')
        server_name auth-service;
        rewrite_log on;
        resolver 127.0.0.11;
        location / {
            auth_request /auth;
            auth_request_set $user $upstream_http_x_user;
            # The variable $user now contains the username when the check was
            # successful
            proxy_pass http://$http_x_instance$uri$is_args$args;
            proxy_set_header X-User $user;
            proxy_set_header X-Original-URI $request_uri;
        }

        location = /auth {
                set $token $cookie_token;
                if ($token = '') {
                  set $token $http_x_token;
                }
                set $instance $http_x_instance;
                if ($instance = '') {
                  return 403;
                }
                proxy_pass http://AUTH_SERVICE/check-token/$token/$instance;
                proxy_pass_request_body off;
                proxy_set_header Content-Length "";
                proxy_set_header X-Original-URI $request_uri;
        }

        define(`expire', `ifelse(COOKIE, `permanent', `; Expires=Fri, 01-Jan-2038 00:00:01 GMT;')')
        location = /login {
                ifdef(`NORATELIMIT', `', `
                limit_req zone=login burst=10 nodelay;')
                proxy_pass http://AUTH_SERVICE/login/;
                proxy_set_header X-Original-URI $request_uri;
                add_header Set-Cookie "token=$upstream_http_x_token; Path=/expire";
        }

        location = /logout {
                proxy_pass http://AUTH_SERVICE/logout/$cookie_token;
                proxy_set_header X-Original-URI $request_uri;
                add_header Set-Cookie "token=deleted; Path=/; Expires=Thu, 01-Jan-1970 00:00:01 GMT";

        }
        location = /disable-sessions {
                set $token $cookie_token;
                if ($token = '') {
                  set $token $http_x_token;
                }
                if ($token = '') {
                  return 403;
                }
                proxy_pass http://AUTH_SERVICE/disable-sessions/$token/;
                proxy_set_header X-Original-URI $request_uri;

        }
        location = /change-password {
                ifdef(`NORATELIMIT', `', `
                limit_req zone=login burst=10 nodelay;')
                set $token $cookie_token;
                if ($token = '') {
                  set $token $http_x_token;
                }
                if ($token = '') {
                  return 403;
                }
                proxy_pass http://AUTH_SERVICE/change-password/$token/;
                proxy_set_header X-Original-URI $request_uri;

        }
        location = /check-token {
                set $token $cookie_token;
                if ($token = '') {
                  set $token $http_x_token;
                }
                if ($token = '') {
                  return 403;
                }
                proxy_pass http://AUTH_SERVICE/check-token/$token/;
                proxy_pass_request_body off;
                proxy_set_header Content-Length "";
                proxy_set_header X-Original-URI $request_uri;
        }


        location = /auth-service.js {
            add_header Content-Type text/javascript;
            alias /www/auth-service.js;
        }

        location = /user-info {
                set $token $cookie_token;
                if ($token = '') {
                  set $token $http_x_token;
                }
                if ($token = '') {
                  return 403;
                }
                proxy_pass http://AUTH_SERVICE/user-info-by-token/$token/;
                proxy_pass_request_body off;
                proxy_set_header Content-Length "";
                proxy_set_header X-Original-URI $request_uri;
        }

        location = /request-password-reset {
                proxy_pass http://AUTH_SERVICE/request-password-reset/;
                proxy_set_header X-Original-URI $request_uri;
        }

        location = /reset-password {
                ifdef(`NORATELIMIT', `', `
                limit_req zone=login burst=3 nodelay;')
                proxy_pass http://AUTH_SERVICE/reset-password/;
                proxy_set_header X-Original-URI $request_uri;
                add_header Set-Cookie "token=$upstream_http_x_token; Path=/EXPIRE";
        }

        location = /reset-password-info {
                ifdef(`NORATELIMIT', `', `
                limit_req zone=login burst=3 nodelay;')
                proxy_pass http://AUTH_SERVICE/reset-password-info/;
                proxy_pass_request_body off;
                proxy_set_header Content-Length "";
                proxy_set_header X-Original-URI $request_uri;
                add_header Set-Cookie "token=$upstream_http_x_token; Path=/EXPIRE";
        }

        # Locations to redirect /auth.html
        location = /authentication/index.html {
            # Serve auth.html instead of a 404 page when auth fails
            error_page 403 =200 /authenticatehtml;
            auth_request /auth;
            # `try_files' only fires after authentication and allows us to use
            # another location if none are found. You can replace @toroot with
            # /, but then it will serve the data from there rather than
            # redirecting
            # We have to do this because both `rewrite' and `return' will
            # short-circuit the authentication request
            try_files nonexistent @toroot;
            # Can use alias to serve a static file instead (will work correctly)
            # alias /www/skip.html;
        }

        location /authentication/ {
            root /www/ ;
            index index.html;
        }

        # Auxiliary location to redirect to / in case of success
        location @toroot {
            return 303 /;
        }
        # Auciliary location to serve the auth.html file
        location = /authenticatehtml {
            internal;
            add_header Content-Type text/html;
            expires -1;
            alias /www/authentication/index.html;
        }


    }
}
