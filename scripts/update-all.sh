#!/bin/bash
echo 'UPPING ALL SERVICES';
echo '--------------------';
docker-compose  pull && docker-compose kill && docker-compose  rm -f && docker-compose  up -d;
echo 'Running Migrations';
echo '------------------';
docker-compose restart migrations-service && docker-compose restart cache resource;