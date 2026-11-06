#!/bin/bash
#
# Reset the search index: remove the elasticsearch, search and kibana
# containers, delete the elasticsearch data, clear the search indexes graph, and
# start the stack again.
#
# Usage:
#   mu script project-scripts reset-search
#
# Needs docker on the host (through the host mount) and the stack's network.

set -uo pipefail

PROJECT_DIR=${PROJECT_DIR:-/project}
SEARCH_INDEXES_GRAPH="http://mu.semte.ch/authorization"
ELASTICSEARCH_DATA="data/elasticsearch"
ENDPOINT=${MU_SPARQL_ENDPOINT:-http://triplestore:8890/sparql}

fail() {
    echo ""
    echo "ERROR: $*" >&2
    exit 1
}

# Ask a yes/no question. Pressing enter answers yes.
confirm() {
    local question=$1
    local answer
    read -r -p "$question [Y/n]: " answer
    [[ -z "$answer" || "$answer" =~ ^[Yy]([Ee][Ss])?$ ]]
}

step() {
    local description=$1
    shift
    echo -n "$description ... "
    if "$@" > /dev/null 2>&1; then
        echo "success"
    else
        echo "failed"
        fail "'$*' returned a non-zero exit code"
    fi
}

# Like step, but returns the exit code so the caller can fall back to another
# command. Nothing is printed, the caller reports the outcome.
step_quiet() {
    "$@" > /dev/null 2>&1
}

clear_graph() {
    curl -sf --max-time 600 -X POST "$ENDPOINT" \
        -H "Content-Type: application/x-www-form-urlencoded" \
        --data-urlencode "update=DELETE WHERE { GRAPH <$1> { ?s ?p ?o } }"
}

ensure_curl() {
    command -v curl > /dev/null && return 0
    apt-get update -qq && apt-get install -y -qq --no-install-recommends curl
}

echo ""
echo "## Resetting the search index"
echo ""
echo "This will:"
echo "  - remove the elasticsearch, search and kibana containers"
echo "  - delete $ELASTICSEARCH_DATA"
echo "  - clear <$SEARCH_INDEXES_GRAPH>"
echo "  - start the stack again"
echo ""
confirm "Continue?" || { echo "Aborted, nothing was changed."; exit 1; }

echo ""
step "installing curl" ensure_curl

echo ""
echo "1. stopping elasticsearch and search"
echo -n "   docker compose rm -fs elasticsearch search kibana ... "
if step_quiet host docker compose rm -fs elasticsearch search kibana; then
    echo "success"
else
    echo "kibana not in the active compose files, retrying without it"
    step "   docker compose rm -fs elasticsearch search" \
        host docker compose rm -fs elasticsearch search
fi

echo "2. removing the elasticsearch data"
step "   deleting $ELASTICSEARCH_DATA" \
    rm -rf "$PROJECT_DIR/$ELASTICSEARCH_DATA"

echo "3. clearing the search indexes graph"
echo -n "   DELETE WHERE on <$SEARCH_INDEXES_GRAPH> ... "
if clear_graph "$SEARCH_INDEXES_GRAPH"; then
    echo "success"
else
    echo "failed"
    fail "could not clear <$SEARCH_INDEXES_GRAPH> at $ENDPOINT"
fi

echo "4. starting the stack"
step "   docker compose up -d --remove-orphans" \
    host docker compose up -d --remove-orphans

echo ""
echo "## Search index reset."
echo ""
echo "Follow the reindex progress with:"
echo ""
echo "  docker compose logs -f search"
