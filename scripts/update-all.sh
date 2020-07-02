#!/bin/bash
echo 'Update and recreate all services (which also runs migrations)';
echo '--------------------';
docker-compose down && docker-compose pull && docker-compose up -d --remove-orphan
