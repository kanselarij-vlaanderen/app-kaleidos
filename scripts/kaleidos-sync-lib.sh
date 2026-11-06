#!/bin/bash
#
# Helpers shared by the kaleidos data sync scripts. Source it, do not run it.
#
# The caller is expected to have set -uo pipefail itself: sourcing this file
# does not change the shell's options.

if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
    echo "ERROR: kaleidos-sync-lib.sh is a library, source it instead." >&2
    exit 1
fi

fail() {
    echo ""
    echo "ERROR: $*" >&2
    exit 1
}

# Print $1 and run the rest, stopping the script if the command fails.
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

# Like step, but keeps the command's output so failures can be explained
# properly instead of being hidden.
step_verbose() {
    local description=$1
    shift
    echo -n "$description ... "
    local output
    if output=$("$@" 2>&1); then
        echo "success"
        return 0
    fi
    echo "failed"
    echo ""
    echo "$output"
    fail "'$*' returned a non-zero exit code"
}

# Ask a yes/no question. Pressing enter answers yes.
confirm() {
    local question=$1
    local answer
    read -r -p "$question [Y/n]: " answer
    [[ -z "$answer" || "$answer" =~ ^[Yy]([Ee][Ss])?$ ]]
}

# Resolve the short hostname without the domain part.
short_hostname() {
    local name
    name=$(hostname -s 2>/dev/null || hostname)
    echo "${name%%.*}"
}

# Run a command on the remote host. IDENTITY_FILE and REMOTE come from the
# caller.
remote() {
    ssh -i "$IDENTITY_FILE" "$REMOTE" "$1"
}

# Retry rsync until it succeeds. Files changing while it runs is expected on a
# live server; anything else is shown on the next attempt.
#
# Progress and errors are streamed as they happen: capturing the output would
# hide the -P progress for the whole run and only show it at the end.
sync_data() {
    local source=$1
    local target=$2
    shift 2

    local attempt=0
    while true; do
        attempt=$((attempt + 1))
        echo "   rsync (attempt $attempt)"
        if rsync -azPh --delete "$@" \
                -e "ssh -i $IDENTITY_FILE" "$source" "$target"; then
            return 0
        fi
        echo "   rsync attempt $attempt failed"
        echo "   retrying in ${RSYNC_RETRY_DELAY}s (ctrl-c to stop)"
        sleep "$RSYNC_RETRY_DELAY"
    done
}

# Wait until a SPARQL endpoint answers a trivial query.
#
# Prints a shell snippet that polls once per interval and returns once the
# triplestore answers, or when the timeout expires. $1 is the prefix that
# reaches isql-v, e.g. "docker compose exec -T triplestore". Run the snippet
# where that prefix works, so one connection covers the whole wait.
triplestore_wait_command() {
    local prefix=$1
    local timeout=${TRIPLESTORE_STARTUP_TIMEOUT:-300}
    local interval=2
    local probe='SPARQL SELECT * WHERE { ?s ?p ?o } LIMIT 1'

    echo "timeout $timeout sh -c 'until $prefix isql-v exec=\"$probe\" > /dev/null 2>&1; do sleep $interval; done'"
}
