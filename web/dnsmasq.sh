#!/bin/sh

# Copyright © 2015-2016 Nejla AB. All rights reserved.

echo "Starting dns"

exec /usr/sbin/dnsmasq -k
