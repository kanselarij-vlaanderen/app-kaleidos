#!/bin/bash
docker-compose  pull && docker-compose  kill && docker-compose  rm -f && docker-compose  up -d