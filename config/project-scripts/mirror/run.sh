#!/bin/bash
#
# Switch a mirror server between live mode and mirror mode, or report which
# mode it is in.
#
# The mode is determined by docker-compose.override.yml in the project folder:
#
#   not a symlink                -> live    (the file is used as-is)
#   symlink to ...live.override  -> live
#   symlink to ...mirror.override-> mirror
#   symlink to anything else     -> unknown (neither mode is guaranteed)
#
# Usage:
#   mu script project-scripts mirror status
#   mu script project-scripts mirror enable-mirror
#   mu script project-scripts mirror enable-live

set -uo pipefail

PROJECT_DIR=${PROJECT_DIR:-/project}

OVERRIDE="docker-compose.override.yml"
LIVE_FILE="docker-compose.live.override.yml"
MIRROR_FILE="docker-compose.mirror.override.yml"

echo ""

usage() {
    echo "Usage: mirror <status|enable-mirror|enable-live>"
    echo ""
    echo "  status          Report whether the stack is in mirror or live mode."
    echo "  enable-mirror   Point $OVERRIDE at $MIRROR_FILE."
    echo "  enable-live     Point $OVERRIDE at $LIVE_FILE."
}

fail() {
    echo "ERROR: $*" >&2
    exit 1
}

# Resolve docker-compose.override.yml to "live", "mirror" or "unknown" and
# print a human readable explanation on the second line.
detect_mode() {
    local override="$PROJECT_DIR/$OVERRIDE"

    if [[ ! -e "$override" && ! -L "$override" ]]; then
        echo "live"
        echo "$OVERRIDE does not exist, so no mirror override is in use"
        return
    fi

    # -L, not -e: a dangling symlink must not be reported as a plain file.
    if [[ ! -L "$override" ]]; then
        echo "live"
        echo "$OVERRIDE is a regular file, not a symlink"
        return
    fi

    local target
    target=$(readlink "$override") || {
        echo "unknown"
        echo "$OVERRIDE is a symlink which cannot be read"
        return
    }
    # Match on the link text and, when it resolves, on the real file name: a
    # dangling link still tells us what it is meant to be.
    local resolved
    resolved=$(readlink -f "$override" 2>/dev/null)
    local resolved_name=${resolved##*/}

    if [[ "${target##*/}" == "$MIRROR_FILE" || "$resolved_name" == "$MIRROR_FILE" ]]; then
        echo "mirror"
        echo "$OVERRIDE -> $target"
    elif [[ "${target##*/}" == "$LIVE_FILE" || "$resolved_name" == "$LIVE_FILE" ]]; then
        echo "live"
        echo "$OVERRIDE -> $target"
    else
        echo "unknown"
        echo "$OVERRIDE -> $target, which is neither $LIVE_FILE nor $MIRROR_FILE"
    fi
}

# Abort unless docker-compose.override.yml can safely be replaced: the file it
# points to must exist, otherwise the stack cannot start.
switch_to() {
    local description=$1
    local target=$2

    cd "$PROJECT_DIR" || fail "cannot enter $PROJECT_DIR"
    [[ -f "$target" ]] || fail "$PROJECT_DIR/$target does not exist; cannot enable $description mode"

    if [[ -e "$OVERRIDE" || -L "$OVERRIDE" ]]; then
        current=$(detect_mode)
        if [[ "${current%%$'\n'*}" == "$description" ]] &&
           [[ "$(readlink "$OVERRIDE" 2>/dev/null)" == "$target" ]]; then
            echo "Already in $description mode ($OVERRIDE -> $target)."
            return 0
        fi
        rm -f "$OVERRIDE" || fail "cannot remove $PROJECT_DIR/$OVERRIDE"
    fi

    ln -s "$target" "$OVERRIDE" || fail "cannot create symlink $OVERRIDE -> $target"
    echo "Enabled $description mode: $OVERRIDE -> $target"
}

command=${1:-}
[[ $# -le 1 ]] || { usage >&2; exit 1; }

case "$command" in
    status)
        mode=$(detect_mode)
        echo "Mode: ${mode%%$'\n'*}"
        echo "${mode#*$'\n'}"
        echo ""
        echo "Restart the stack for a change to take effect: docker compose up -d"
        ;;
    enable-mirror)
        switch_to mirror "$MIRROR_FILE"
        ;;
    enable-live)
        switch_to live "$LIVE_FILE"
        ;;
    -h|--help|help)
        usage
        ;;
    *)
        usage >&2
        exit 1
        ;;
esac
