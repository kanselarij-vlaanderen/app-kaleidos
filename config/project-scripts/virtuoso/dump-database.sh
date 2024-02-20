#!/bin/bash
USERNAME=${2:-"dba"}
PASSWORD=${3:-"dba"}
TRIPLESTORE=${1:-"triplestore"}

if [[ "$#" -ge 3 ]]; then
    echo "Usage:"
    echo "   mu script triplestore [hostname] [username] [password]"
    exit -1;
fi

if [[ -d "/project/data/db" ]];then
    mkdir -p /project/data/db/dumps
else
    echo "WARNING:"
    echo "    did not find data/db folder in your project, so did not create data/db/dumps!"
    echo " "
fi


echo "connecting to $TRIPLESTORE with $USERNAME"
isql-v -H $TRIPLESTORE -U $USERNAME -P $PASSWORD <<EOF
    dump_nquads ('dumps', 1, 100000000, 1);
    exit;
EOF
gunzip /project/data/db/dumps/*.gz
cat /project/data/db/dumps/* > /project/data/dumped-quads.nq
