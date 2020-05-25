#!/bin/bash

if [ $# -lt 1 ]
then
    echo "Uploads the file in $1 to the sparqlendpoint $2 or http://localhost:8890 by sparql query"
    exit
fi

if [ ! -d kaleidos-downloads ]
then
    mkdir kaleidos-downloads
fi

sparqlendpoint="http://localhost:8890"
if [ $# -ge 2 ]
then
  sparqlendpoint=$2
fi
ttl=`cat $1`
graph="http://mu.semte.ch/graphs/organizations/kanselarij"
if [ $# -ge 3 ]
then
  graph=$3
fi

echo "update=INSERT DATA { GRAPH <$graph> { $ttl } }" > kaleidos-downloads/tmp
curl "$sparqlendpoint/sparql/" \
  -H 'Connection: keep-alive' \
  -H 'mu-auth-sudo: true' \
  -H 'Accept: text/html' \
  -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/81.0.4044.113 Safari/537.36' \
  -H 'Content-Type: application/x-www-form-urlencoded' \
  -H 'Origin: http://localhost:8890' \
  -H 'Sec-Fetch-Site: same-origin' \
  -H 'Sec-Fetch-Mode: cors' \
  -H 'Sec-Fetch-Dest: empty' \
  -H 'Referer: http://localhost:8889/conductor/sparql_input.vspx?sid=636e5e3252315527bfb05e52d79b2458&realm=virtuoso_admin' \
  -H 'Accept-Language: en-US,en;q=0.9,nl;q=0.8,en-GB;q=0.7,fr;q=0.6' \
  -H 'Cookie: keystone.sid=s%3AEPfnu2gekLXb8YTft7zF1xEBbLhSDeTh.7k2aR%2B8O6SaSPhh%2Bb5R6GIMIL8BspzTUpWQWifNqor0; proxy_session=QTEyOEdDTQ.A8g0knbYudyZNipvP-Rk3l-99LP7xj7zN0_XLcHUX7VxxT-e2h2DjK0lUQk.t8hDPGTsHB26B8Uo.wOHpIN00wsNYuhNcrenxf1yPM-nfMbelyqB4aQkQBlk6TlE39wb7Zb3vCVH1SsDyGBcfqKYAfKhDWE32SQTteQJdbuZxNuU6GFWzNX1LcMnaukvxrNrAaZrDZethK4x3gLSx1yIdjcHGSWXQViVvnTSQRaFbf5Q3VE3HMEQSuIFbxhYoNn2v-cBdie5HFUTk8aaiZyPj_xK9GPRF5oEjqnSjket396-qGosxuFVleiPdcxiLeHkCMHMoh1gsn0-R8jmTvEmJRp4OMMpd8MpcEIXrsmqmk9jXM7675PjScrwirMjcqKCzCsLFygr-PE8yoz1cW9F7hHAQ9nwuA12joYavu9ikwnvOCaPFCJRiL7ImJxPHLDHCXvcdIhpsUtP6leXFmX3zuiQK15ZCji7HsIiZpTwJ7UBrxIwb79VNCGS-xVHxnLdwMZJZnOFGxybqUQ.NNQBhN1l8m_fpvl0-UPB1Q' \
  -d @kaleidos-downloads/tmp

echo ""
echo "done"


