#!/usr/bin/env bash

# We use bash because it's already installed in the nginx container
# Python would add ~30MB
#
# Mustache ads ~1MB (only marginally more than m4)
# Gomplate would be more flexible than mustache but comes in at roughly 50MB

set -e

generate_config () {

SESSION_COOKIES=$(echo "$SESSION_COOKIES" | tr '[:upper:]' '[:lower:]')

if [[ "$SESSION_COOKIES" = "true" ]]; then
  # Don't set cookie expiration, which means the cookies will be deleted on browser close
  expire=""
elif [[ -z "$SESSION_COOKIES" ]] || [[ "$SESSION_COOKIES" = "false" ]]; then
  expire=" Expires=Fri, 01-Jan-2038 00:00:01 GMT;"
else
    echo "SESSION_COOKIES variable must be one of 'true' or 'false'"
    exit 1
fi

if [[ -n "$NORATELIMIT" ]]; then
  ratelimit="false"
else
  # Default to rate limitting enabled
  ratelimit="true"
fi

declare -a inst_ports=()

# Handle instance ports as a list
#
# Example:
# INSTANCE_PORTS=657b5108-7559-4b8e-a643-dd0cc29b9e34=80;63ebe974-a364-4755-9cfd-303470dee418=8080
#
# We don't need to set instance URIs because hostnames can be aliased in the docker-compose.yaml

# Split INSTANCE_PORTS along semicolons
IFS=";" read -ra insts <<<"${INSTANCE_PORTS}"
for inst in "${insts[@]}"; do
  # Match {instance}={port}
  if [[ $inst =~ ^([^_]+)=(.+)$ ]]; then
    inst_ports+=("{\"instance\": \"${BASH_REMATCH[1]}\", \"port\": \"${BASH_REMATCH[2]}\"}")
  fi
done

if [[ $DEBUG = true ]]; then
  NGINX=nginx-debug
else
  NGINX=nginx
fi

IFS=,
cat > nginx.conf.json <<EOF
{
  "auth_service_host": "${AUTH_SERVICE:-auth-service-backend:80}",
  "ratelimit": $ratelimit,
  "expire": "$expire",
  "access_log": ${ACCESS_LOG:-false},
  "error_log": ${ERROR_LOG:-false},
  "debug": ${DEBUG:-false},
  "instance_ports": [${inst_ports[*]}]
  }
EOF
}

case "$1" in
  test)
    generate_config
    ./mustache --allow-missing-variables=false nginx.conf.json proxy/nginx.conf.mustache
    ;;
  *)
    generate_config
    echo "Generating config using:"
    cat nginx.conf.json
    /opt/mustache --allow-missing-variables=false nginx.conf.json nginx.conf.mustache > /etc/nginx/nginx.conf
    echo "Authentication web server starting"
    echo "${NGINX[@]}" -g "daemon off;"
    cd /etc/nginx && exec "${NGINX[@]}" -g "daemon off;"
    ;;
esac
