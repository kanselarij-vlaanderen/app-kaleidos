#!/bin/bash
#
# Reset the test data: stop the services that read the data, replace data with
# the contents of testdata.zip and testdata-elasticsearch.zip, and start the
# stack again.
#
# Usage:
#   mu script project-scripts reset-test
#
# Needs docker on the host (through the host mount), the stack's network and
# the ability to install packages.

set -uo pipefail

PROJECT_DIR=${PROJECT_DIR:-/project}
TEST_DATA="testdata.zip"
SEARCH_DATA="testdata-elasticsearch.zip"

STOPPED_SERVICES=(
    yggdrasil triplestore elasticsearch search
    file public-file themis-export export-file ttl-to-delta range-file
    file-bundling publication-report decision-report-generation
    decision-extraction minutes-report-generation digital-signing
    document-stamping pdf-signature-remover pdf-flattener draft-file
    draft-file-mover delta-consumer resource cache cache-warmup forever-cache
)
STARTED_SERVICES=(
    triplestore database migrations resource cache forever-cache
)

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

# Run a host command, which mu-cli only allows from a subshell with a matching
# permission, so no function may wrap it. Returns the exit code instead of
# failing, the caller decides what to do.
host_step() {
    local description=$1
    shift
    echo -n "$description ... "
    if ( "$@" ) > /dev/null 2>&1; then
        echo "success"
        return 0
    fi
    echo "failed"
    return 1
}

stop_service() {
    echo -n "   stopping $1 ... "
    if ( host docker compose rm -fs "$1" ) > /dev/null 2>&1; then
        echo "success"
    else
        echo "not running"
    fi
}

# Replace target with source. Leaving the old folder in place would make mv
# put source inside target.
replace_with_unpacked() {
    local source=$1
    local target=$2

    [[ -d "$source" ]] || fail "$source is not in the archives"
    step "   replacing ${target#"$PROJECT_DIR/"}" rm -rf "$target"
    step "   moving ${source#"$PROJECT_DIR/"}" mv "$source" "$target"
}

poll_triplestore() {
    local attempt
    for ((attempt = 1; attempt <= 60; attempt++)); do
        if curl -sf --max-time 10 -G "http://triplestore:8890/sparql" \
                --data-urlencode "query=ASK {}" > /dev/null 2>&1; then
            return 0
        fi
        sleep 2
    done
    return 1
}

[[ $# -eq 0 ]] || {
    echo "Usage: mu script project-scripts reset-test" >&2
    exit 1
}

for archive in "$TEST_DATA" "$SEARCH_DATA"; do
    [[ -f "$PROJECT_DIR/$archive" ]] || fail "$archive not found in the project folder"
done

echo ""
echo "## Resetting the test data"
echo ""
echo "This will:"
echo "  - stop the services that use the database, the files and the search index"
echo "  - replace data/db, data/files and data/elasticsearch with the contents of"
echo "    $TEST_DATA and $SEARCH_DATA"
echo "  - start the stack again"
echo ""
confirm "Continue?" || { echo "Aborted, nothing was changed."; exit 1; }

if ! command -v unzip > /dev/null 2>&1; then
    echo ""
    echo "Installing unzip"
    apt-get update > /dev/null 2>&1 &&
        apt-get install -y unzip > /dev/null 2>&1 ||
        fail "could not install unzip"
fi

echo ""
echo "1. stopping the services that use the data"
for service in "${STOPPED_SERVICES[@]}"; do
    stop_service "$service"
done

step "2. deleting data/db, data/files, data/tika and data/elasticsearch" \
    rm -rf "$PROJECT_DIR/data/db" "$PROJECT_DIR/data/files" "$PROJECT_DIR/data/tika" \
        "$PROJECT_DIR/data/elasticsearch"

step "3. unpacking $TEST_DATA" unzip -q -o "$PROJECT_DIR/$TEST_DATA" -d "$PROJECT_DIR"
replace_with_unpacked "$PROJECT_DIR/testdata/db" "$PROJECT_DIR/data/db"
replace_with_unpacked "$PROJECT_DIR/testdata/files" "$PROJECT_DIR/data/files"

step "4. unpacking $SEARCH_DATA" \
    unzip -q -o "$PROJECT_DIR/$SEARCH_DATA" -d "$PROJECT_DIR"
replace_with_unpacked "$PROJECT_DIR/testdata-elasticsearch/elasticsearch" \
    "$PROJECT_DIR/data/elasticsearch"
step "   creating data/tika/cache" mkdir -p "$PROJECT_DIR/data/tika/cache"
step "   copying testdata/tika/cache" \
    cp -a "$PROJECT_DIR/testdata/tika/cache/." "$PROJECT_DIR/data/tika/cache/"
step "   deleting the unpacked test data" \
    rm -rf "$PROJECT_DIR/testdata" "$PROJECT_DIR/testdata-elasticsearch"

step "5. starting triplestore and database" \
    host docker compose up -d "${STARTED_SERVICES[@]}"
echo "   waiting for the triplestore to accept queries"
if poll_triplestore; then
    step "6. starting the stack" \
        host docker compose up -d --remove-orphans
else
    echo "   the triplestore is not answering yet, starting the stack without waiting"
    host_step "6. starting the stack" \
        host docker compose up -d --remove-orphans ||
        fail "could not start the stack"
fi

echo ""
echo "## Test data reset."
echo ""
