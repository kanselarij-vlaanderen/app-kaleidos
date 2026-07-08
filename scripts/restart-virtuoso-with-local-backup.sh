#!/bin/bash
docker=`which docker`
if [ $? -ne 0 ]; then
    echo "ERROR: could not find docker executable";
    exit -1;
fi

mu=`which mu`
if [ $? -ne 0 ]; then
    echo "ERROR: could not find mu-cli";
    exit -1;
fi

cid="$(docker compose ps -q triplestore || true)"
if [ -z "$cid" ]; then
  echo "ERROR: triplestore service is not running (no container found)";
  exit -1;
fi

echo "Taking checkpoint before local backup"
$mu script triplestore maintenance checkpoint

echo "Taking local DB backup"
$mu script project-scripts local-db-copy kaleidos-db-backup-before-restart

echo "Shutting down virtuoso (docker will restart)"
$docker compose exec -T triplestore isql-v <<EOF
    exec('shutdown');
    exit;
EOF

echo "Virtuoso has been shutdown. Container will restart automatically (unless otherwise configured)."
