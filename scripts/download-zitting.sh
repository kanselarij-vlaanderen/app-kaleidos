#!/bin/bash


NUMARGS=2

if [ ! -d kaleidos-downloads ]
then
    mkdir kaleidos-downloads
fi

if [ $# -lt "$NUMARGS" ]
then
    echo "Downloads all data related to a 'zitting' from an yggdrasil endpoint and loads it into your sparql endpoint"
    echo ""
    echo "Usage: ./download-zitting.sh http://yggdrasilhost:port <zitting-id> [anon] [sparqlendpoint=http://localhost:8890] [targetgraph=http://mu.semte.ch/graphs/organizations/kanselarij] [--dry-run]"
    echo ""
    echo "NOTE:"
    echo "- anon is optional, if present, data will be anonymized by replacing all string values by the uri"
    echo "- this may download sensitive data from a production environment. Thread carefully!"
    echo "- data will be stored in ./kaleidos-downloads, so you can undo the upload again"
    echo "- by default, this endpoint will be disabled in yggdrasil, the port will also only be open on the server, not exposed to the outside"
    echo "- to enable the endpoint, add the ALLOW_DOWNLOADS=true environment variabple to yggdrasil"
    echo "- to be able to call the endpoint, make a tunnel to the server using ssh"
    echo "- --dry-run only downloads the data as a file, it doesn't upload it to an endpoint"
  exit 1
fi

sparqlendpointpos=3
graphpos=4
anon="false"


if [ $# -ge 3 ]
then
    if [ $3 == "anon" ]; then
        anon="true"
        sparqlendpointpos=4
        graphpos=5
    fi
fi

sparqlendpoint="http://localhost:8890"

if [ $# -ge $sparqlendpointpos ]
then
  sparqlendpoint=$$sparqlendpointpos
fi

graph="http://mu.semte.ch/graphs/organizations/kanselarij"
if [ $# -ge $graphpos ]
then
  graph=$$graphpos
fi

yggdrasilhost=$1
zittingid=$2

echo "storing zitting data for $zittingid in <$graph> of endpoint $sparqlendpoint..."
echo "starting download of zitting $zittingid in 3 seconds, press Ctrl-C to cancel"
if [ $anon == "true" ]; then
    echo "anonymizing data"
else
    echo "not anonymizing data"
fi
echo "3..."
sleep 1
echo "2..."
sleep 1
echo "1..."
sleep 1
echo -n "fetching zitting data for $zittingid..."
res=$(curl -s -w "%{http_code}" $yggdrasilhost/downloadZitting?zitting=$zittingid\&anonymize=$anon)
if [ ${#res} -lt 4 ]; then
    echo ""
    echo "Error fetching data:  invalid response from server"
    echo $res
    exit 1
fi
downloadId=${res::${#res}-3}
status=$(echo $res | tail -c 4)
if [ "$status" -ne "200" ]; then
    echo ""
    echo "Error fetching data:  $status"
    echo $ttl
    exit $status
fi
alldone=0
while [ "$alldone" -eq 0 ]
do
    res=$(curl -s -w "%{http_code}" $yggdrasilhost/downloadZittingResult?id=$downloadId)
    if [ ${#res} -lt 4 ]; then
        echo ""
        echo "Error fetching result:  invalid response from server"
        echo $res
        exit 1
    fi
    ttl=${res::${#res}-3}
    status=$(echo $res | tail -c 4)
    if [ "$status" -ne "200" ]; then
        echo ""
        echo "Error fetching result:  $status"
        echo $ttl
        exit $status
    fi
    if [ "$ttl" != "loading" ]; then
        alldone=1
    fi
    sleep 1
    echo -n "."
done
filename="kaleidos-downloads/$zittingid-$(date +"%Y-%m-%d-%H-%M-%S").ttl"
echo $ttl > $filename
echo "done"
echo ""
echo "done, written to $filename"
triplesretrieved=$(echo "$ttl" | wc -l | xargs)
echo "$triplesretrieved triples retrieved"
echo ""
if [ "${@: -1}" == "--dry-run" ]; then
    echo "Ended without uploading data" 
    exit 0
fi

echo -n "storing data..."
linecount=0
totallines=0
lines=""
while IFS= read -r line
do
    lines="$lines
$line"
    linecount=$((linecount + 1))
    totallines=$((totallines + 1))
    if [ "$linecount" -gt 1000 ]; then
        # need to write to file because otherwise argument list can be too long...
        echo "INSERT DATA { GRAPH <$graph> { $lines } }" > kaleidos-downloads/tmp
        curl -s -H "cache-control: no-cache" -H "host: database:8890" -H "mu-auth-sudo: true" -H "connection: keep-alive" -H "Accept: *" --form "content-type=text/csv" --form "format=text/csv" --form "update=<kaleidos-downloads/tmp" -X POST "$sparqlendpoint/sparql" > /dev/null
        echo -n "."
        linecount=0
        lines=""
    fi
done <<< "$ttl"
if [ "$linecount" -gt 0 ]; then
    echo "INSERT DATA { GRAPH <$graph> { $lines } }" > kaleidos-downloads/tmp
    curl -s -H "cache-control: no-cache" -H "host: database:8890" -H "mu-auth-sudo: true" -H "connection: keep-alive" -H "Accept: *" --form "content-type=text/csv" --form "format=text/csv" --form "update=<kaleidos-downloads/tmp" -X POST "$sparqlendpoint/sparql" > /dev/null
fi

echo "done"
echo "inserted $totallines (or less) triples"
echo ""
rm kaleidos-downloads/tmp
echo "all done"


