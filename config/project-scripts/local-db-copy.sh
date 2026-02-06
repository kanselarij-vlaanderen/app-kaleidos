#!/bin/bash
date=`date +%y%m%dT%H%M`
backup_folder="${1:-kaleidos-db-backup-$date}"
mkdir -p /project/$backup_folder
cp -r /project/data/db/ /project/$backup_folder
echo ""
echo "Local copy of data/db written to ./$backup_folder"
