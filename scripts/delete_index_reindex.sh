#!/bin/bash

echo 'this script is for reference only';
echo 'first step:';
echo 'add 127.0.0.1:7070:80 to the ports section of musearch';
echo 'then change the correct variables to reindex';
echo 'then execute the curl call from the script';

exit;

#  musearch:
#    ports:
#      - 127.0.0.1:7070:80

curl -X DELETE -H "Content-length: 0" -H "mu-auth-allowed-groups: [{\"variables\":[],\"name\":\"clean\"},{\"variables\":[\"intern-regering\"],\"name\":\"o-intern-regering-read\"},{\"variables\":[],\"name\":\"public\"}]"  localhost:7070/cases/delete