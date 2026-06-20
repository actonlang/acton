#!/bin/sh
# Start tinyproxy (it daemonizes) and stream its log to container stdout so
# `docker compose logs proxy` shows every CONNECT.
set -e
mkdir -p /var/log/tinyproxy
: > /var/log/tinyproxy/tinyproxy.log
tinyproxy -c /etc/tinyproxy/tinyproxy.conf
echo "[proxy] tinyproxy started on :8888"
exec tail -F /var/log/tinyproxy/tinyproxy.log
