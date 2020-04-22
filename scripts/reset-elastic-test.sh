#!/bin/bash
echo "warning this will run queries on the triplestore and delete containers, you have 5 seconds to press ctrl+c"
sleep 5
docker-compose -f docker-compose.yml -f docker-compose.development.yml -f docker-compose.test.yml -f docker-compose.override.yml rm -fs elasticsearch  musearch
rm -rf testdata/elasticsearch
docker-compose -f docker-compose.yml -f docker-compose.development.yml -f docker-compose.test.yml -f docker-compose.override.yml exec -T triplestore isql-v <<EOF
SPARQL DELETE WHERE {   GRAPH <http://mu.semte.ch/authorization> {     ?s ?p ?o.   } };
exec('checkpoint');
exit;
EOF
docker-compose -f docker-compose.yml -f docker-compose.development.yml -f docker-compose.test.yml -f docker-compose.override.yml up -d  --remove-orphans