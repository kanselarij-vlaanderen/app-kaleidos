#!/bin/bash
if [ $# -eq 0 ]; then
    echo "ERROR: No argument provided. Usage: $0 <query_file>";
    exit -1;
fi
query_file="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"

if [ ! -f "$query_file" ]; then
    echo "ERROR: File $query_file doesn't exist";
    exit -1;
fi

docker=`which docker`
if [ $? -ne 0 ]; then
    echo "ERROR: could not find docker executable";
    exit -1;
fi

cid="$(docker compose ps -q triplestore || true)"
if [ -z "$cid" ]; then
  echo "ERROR: triplestore service is not running (no container found)";
  exit -1;
fi

echo "Executing SPARQL query $query_file"
$docker compose exec -T triplestore isql-v exec="SPARQL $(cat $query_file)"
