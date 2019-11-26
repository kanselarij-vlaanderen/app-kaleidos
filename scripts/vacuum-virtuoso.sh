#!/bin/bash

if [ $# -ne 1 ];then
    echo "Usage: "
    echo "      $0 [virtuoso-container-name]"
    exit -1;
fi

docker=`which docker`
if [ $? -ne 0 ]; then
    echo "ERROR: could not find docker executable";
    exit -1;
fi


virtuoso_container=$1
$docker exec -i $virtuoso_container isql-v <<EOF
    exec('checkpoint');
    DB.DBA.vacuum();
    exec('checkpoint');
    exit;
EOF
