#!/bin/bash
#
# Script to sync data from a main server to its mirror.
#
# The target is derived from the hostname this script runs on:
#
#   kal-clio -> kal-charon   /data/app-kaleidos-prod -> /data/app-kaleidos-prod-mirror
#   kal-bia  -> kal-boreas   /data/app-kaleidos-test -> /data/app-kaleidos-test-mirror
#
# The script asks for confirmation before it changes anything; the mirror stack
# is stopped and its data replaced.

set -uo pipefail

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
[[ -f "$SCRIPT_DIR/kaleidos-sync-lib.sh" ]] \
    || { echo "ERROR: kaleidos-sync-lib.sh not found in $SCRIPT_DIR" >&2; exit 1; }
# shellcheck source=kaleidos-sync-lib.sh
. "$SCRIPT_DIR/kaleidos-sync-lib.sh"

IDENTITY_FILE="/root/.ssh/id_rsa_mirror_sync"

# Seconds between rsync attempts when files change while it runs.
RSYNC_RETRY_DELAY=${RSYNC_RETRY_DELAY:-10}

usage() {
    echo "Usage: $0 [-h|--help]"
    echo ""
    echo "Syncs this server's data to its mirror."
    echo "The mirror is derived from the hostname, no arguments are taken."
    echo ""
    echo "Supported hosts:"
    echo "  kal-clio -> kal-charon  (/data/app-kaleidos-prod -> /data/app-kaleidos-prod-mirror)"
    echo "  kal-bia  -> kal-boreas  (/data/app-kaleidos-test -> /data/app-kaleidos-test-mirror)"
    echo ""
    echo "Set RSYNC_RETRY_DELAY to change the wait between rsync attempts"
    echo "(default ${RSYNC_RETRY_DELAY}s)."
}

# ---------------------------------------------------------------------------
# Determine source and mirror from the hostname
# ---------------------------------------------------------------------------

HOSTNAME_SHORT=$(short_hostname)

case "$HOSTNAME_SHORT" in
    kal-clio)
        LOCAL_DIR="/data/app-kaleidos-prod"
        REMOTE_TARGET="kal-charon"
        REMOTE_DIR="/data/app-kaleidos-prod-mirror"
        ;;
    kal-bia)
        LOCAL_DIR="/data/app-kaleidos-test"
        REMOTE_TARGET="kal-boreas"
        REMOTE_DIR="/data/app-kaleidos-test-mirror"
        ;;
    *)
        echo "This script does not run on '$HOSTNAME_SHORT'." >&2
        echo "" >&2
        usage >&2
        exit 1
        ;;
esac

REMOTE="root@$REMOTE_TARGET.s.redhost.be"

# Downstream consumers (cron jobs reading this script) may call it explicitly
# with the target; that is harmless as long as it agrees with the hostname.
if [[ $# -gt 0 ]]; then
    case "$1" in
        -h|--help)
            usage
            exit 0
            ;;
        "$REMOTE_TARGET")
            shift
            ;;
        *)
            echo "'$1' does not match this host: $HOSTNAME_SHORT syncs to $REMOTE_TARGET." >&2
            exit 1
            ;;
    esac
fi
[[ $# -eq 0 ]] || { usage >&2; exit 1; }

# ---------------------------------------------------------------------------

[[ -d "$LOCAL_DIR/data" ]] || fail "$LOCAL_DIR/data does not exist; is this the right host?"
[[ -f "$IDENTITY_FILE" ]] || fail "SSH key $IDENTITY_FILE not found"

# mu runs on the mirror, over SSH, so only the local tools are checked here.
for tool in rsync ssh; do
    command -v "$tool" > /dev/null || fail "'$tool' is required but not installed"
done

# Local time is used for the backup name and for the delta timestamp: the
# mirror runs in the same timezone, and local timestamps sort correctly.
TIMESTAMP=$(date +%Y%m%dT%H%M%S)
BACKUP_NAME="kaleidos-db-backup-$TIMESTAMP"
BACKUP_DIR="$REMOTE_DIR/$BACKUP_NAME"

# The copy start time is slightly earlier than the rsync it describes: the
# remote backup is taken before rsync, so deltas from this timestamp are all
# covered. Without a UTC offset the consumer reads it as local time, which is
# what we mean.
COPY_START=$(date +%Y-%m-%dT%H:%M:%S)

# Validate the plan before anything is changed.
check_local_source() { [[ -d "$LOCAL_DIR/data" ]]; }
check_remote_mirror() { ssh -o BatchMode=yes -i "$IDENTITY_FILE" "$REMOTE" "test -d '$REMOTE_DIR'"; }

check_remote_mu() {
    remote "command -v mu" > /dev/null
}

echo "## Data sync $HOSTNAME_SHORT -> $REMOTE_TARGET"
echo ""
echo "  source:   $LOCAL_DIR/data"
echo "  mirror:   $REMOTE:$REMOTE_DIR"
echo "  backup:   $REMOTE:$BACKUP_DIR"
echo "  start of copy: $COPY_START"
echo ""
echo "The mirror stack will be stopped and its data replaced."
if ! confirm "Continue?"; then
    echo "Aborted, nothing was changed."
    exit 1
fi

echo ""
echo "1. testing remote connection"
step_verbose "1. reaching $REMOTE" \
    ssh -o BatchMode=yes -i "$IDENTITY_FILE" "$REMOTE" "true"
step_verbose "   checking mirror folder" \
    check_remote_mirror
if ! check_remote_mu; then
    echo ""
    fail "mu is not on the PATH on $REMOTE_TARGET."
fi

echo "2. stopping the mirror stack"
step_verbose "   docker compose down" \
    remote "cd '$REMOTE_DIR' && docker compose down"

echo "3. creating db backup on the mirror"
step_verbose "   docker compose up -d project-scripts" \
    remote "cd '$REMOTE_DIR' && docker compose up -d project-scripts"
step_verbose "   mu script project-scripts local-db-copy $BACKUP_NAME" \
    remote "cd '$REMOTE_DIR' && mu script project-scripts local-db-copy '$BACKUP_NAME'"
step_verbose "   checking the backup exists" \
    ssh -i "$IDENTITY_FILE" "$REMOTE" "test -d '$BACKUP_DIR'"

echo "4. syncing data, overwriting data on $REMOTE"
# Every file that comes from the source this way is excluded, so a failed
# rsync cannot have deleted the backup on its retry.
sync_data "$LOCAL_DIR/data" "$REMOTE:$REMOTE_DIR/" --exclude "/$BACKUP_NAME/"

echo ""
echo "## data sync to $REMOTE completed, the mirror stack is still down."
echo ""
echo "Ensure the mirror is configured to behave as a mirror and start the stack."
echo "On $REMOTE_TARGET:"
echo ""
echo "  cd $REMOTE_DIR"
echo "  mu script project-scripts mirror enable-mirror"
echo "  mu script project-scripts mirror status"
echo ""
echo "Set DCR_START_FROM_DELTA_TIMESTAMP to the start time of the copy:"
echo "  DCR_START_FROM_DELTA_TIMESTAMP=\"$COPY_START\""
echo ""
echo "Then start the stack: drc up -d"
echo ""
echo "Remove the backup once you verified the sync went well:"
echo "  rm -rf $BACKUP_DIR"
echo ""
echo "(On $REMOTE_TARGET that is: ssh $REMOTE \"rm -rf '$BACKUP_DIR'\")"
