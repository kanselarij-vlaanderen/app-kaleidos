#!/bin/bash
#
# Script to copy the data of this server onto another environment.
#
# The source is derived from the hostname this script runs on; the target and
# a notification email address are given as arguments:
#
#   kal-clio   /data/app-kaleidos-prod
#   kal-charon /data/app-kaleidos-prod-mirror
#   kal-bia    /data/app-kaleidos-test
#   kal-boreas /data/app-kaleidos-test-mirror
#
# The script asks for confirmation before it changes anything; the target is
# stopped and its data replaced. Users of the target are kept.

set -uo pipefail

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
[[ -f "$SCRIPT_DIR/kaleidos-sync-lib.sh" ]] \
    || { echo "ERROR: kaleidos-sync-lib.sh not found in $SCRIPT_DIR" >&2; exit 1; }
# shellcheck source=kaleidos-sync-lib.sh
. "$SCRIPT_DIR/kaleidos-sync-lib.sh"

IDENTITY_FILE="/root/.ssh/id_rsa_sync"

# Seconds between rsync attempts when files change while it runs.
RSYNC_RETRY_DELAY=${RSYNC_RETRY_DELAY:-10}

usage() {
    echo "Usage: $0 [-h|--help] <target> <notification-email-address>"
    echo ""
    echo "Copies this server's data to <target>, keeping the target's users,"
    echo "then points the target's notification emails at the given address."
    echo ""
    echo "Supported targets:"
    echo "  kal-clio    /data/app-kaleidos-prod"
    echo "  kal-charon  /data/app-kaleidos-prod-mirror"
    echo "  kal-bia     /data/app-kaleidos-test"
    echo "  kal-boreas  /data/app-kaleidos-test-mirror"
    echo ""
    echo "Set RSYNC_RETRY_DELAY to change the wait between rsync attempts"
    echo "(default ${RSYNC_RETRY_DELAY}s)."
}

# ---------------------------------------------------------------------------
# Determine source from the hostname and target from the argument
# ---------------------------------------------------------------------------

HOSTNAME_SHORT=$(short_hostname)

case "$HOSTNAME_SHORT" in
    kal-clio)   LOCAL_DIR="/data/app-kaleidos-prod" ;;
    kal-charon) LOCAL_DIR="/data/app-kaleidos-prod-mirror" ;;
    kal-bia)    LOCAL_DIR="/data/app-kaleidos-test" ;;
    kal-boreas) LOCAL_DIR="/data/app-kaleidos-test-mirror" ;;
    *)
        echo "This script does not run on '$HOSTNAME_SHORT'." >&2
        echo "" >&2
        usage >&2
        exit 1
        ;;
esac

target_dir_for() {
    case "$1" in
        kal-clio)   echo "/data/app-kaleidos-prod" ;;
        kal-charon) echo "/data/app-kaleidos-prod-mirror" ;;
        kal-bia)    echo "/data/app-kaleidos-test" ;;
        kal-boreas) echo "/data/app-kaleidos-test-mirror" ;;
        *)          return 1 ;;
    esac
}

if [[ $# -gt 0 && ( "$1" == "-h" || "$1" == "--help" ) ]]; then
    usage
    exit 0
fi

[[ $# -eq 2 ]] || { usage >&2; exit 1; }

REMOTE_TARGET=$1
NOTIFICATION_EMAIL=$2
REMOTE_DIR=$(target_dir_for "$REMOTE_TARGET") \
    || fail "'$REMOTE_TARGET' is not a supported target; see the usage above."
REMOTE="root@$REMOTE_TARGET.s.redhost.be"

if [[ "$REMOTE_DIR" == "$LOCAL_DIR" ]]; then
    fail "source and target are both $LOCAL_DIR; pick another target."
fi

# ---------------------------------------------------------------------------

[[ -d "$LOCAL_DIR/data" ]] || fail "$LOCAL_DIR/data does not exist; is this the right host?"
[[ -f "$IDENTITY_FILE" ]] || fail "SSH key $IDENTITY_FILE not found"

# mu runs in containers on both sides, over SSH for the target, so only the
# local tools are checked here.
for tool in rsync ssh; do
    command -v "$tool" > /dev/null || fail "'$tool' is required but not installed"
done

TIMESTAMP=$(date +%Y%m%dT%H%M%S)
BACKUP_NAME="kaleidos-db-backup-$TIMESTAMP"
BACKUP_DIR="$REMOTE_DIR/$BACKUP_NAME"
USERS_BACKUP_NAME="kaleidos-users-backup-$TIMESTAMP.nq"
USERS_BACKUP_DIR="$REMOTE_DIR/$USERS_BACKUP_NAME"

remote_mu() {
    local args
    args=$(printf '%q ' "$@")
    remote "cd '$REMOTE_DIR' && mu script project-scripts data-sync $args"
}

wait_for_remote_triplestore() {
    remote "cd '$REMOTE_DIR' && $(triplestore_wait_command 'docker compose exec -T triplestore')"
}

check_remote_target() {
    ssh -o BatchMode=yes -i "$IDENTITY_FILE" "$REMOTE" \
        "test -d '$REMOTE_DIR' && command -v mu > /dev/null"
}

echo "## Data sync $HOSTNAME_SHORT -> $REMOTE_TARGET"
echo ""
echo "  from:     $HOSTNAME_SHORT:$LOCAL_DIR/data"
echo "  to:       $REMOTE_TARGET:$REMOTE_DIR/data"
echo "  backup:   $REMOTE_TARGET:$BACKUP_DIR"
echo "  users:    $REMOTE_TARGET:$USERS_BACKUP_DIR"
echo "  notifications to: $NOTIFICATION_EMAIL"
echo ""
echo "The target's stack is stopped and its data replaced."
echo "The target's users are backed up and put back afterwards."
if ! confirm "Continue?"; then
    echo "Aborted, nothing was changed."
    exit 1
fi

echo ""
echo "1. testing remote connection"
step_verbose "1. reaching $REMOTE" \
    ssh -o BatchMode=yes -i "$IDENTITY_FILE" "$REMOTE" "true"
step "   checking target folder and mu" \
    check_remote_target

echo "2. creating users backup on the target"
step "   docker compose up -d project-scripts" \
    remote "cd '$REMOTE_DIR' && docker compose up -d project-scripts"
# The triplestore has to be up to read the users graph; this runs before the
# stack is stopped so this is the only start it needs.
step "   docker compose up -d triplestore" \
    remote "cd '$REMOTE_DIR' && docker compose up -d triplestore"
step_verbose "   waiting for the triplestore to accept queries" \
    wait_for_remote_triplestore
step_verbose "   mu script project-scripts data-sync backup-users $USERS_BACKUP_NAME" \
    remote_mu backup-users "$USERS_BACKUP_NAME"

echo "3. stopping the stack on the target"
step "   docker compose down" \
    remote "cd '$REMOTE_DIR' && docker compose down"

echo "4. creating db backup on the target"
step "   docker compose up -d project-scripts" \
    remote "cd '$REMOTE_DIR' && docker compose up -d project-scripts"
step "   mu script project-scripts local-db-copy $BACKUP_NAME" \
    remote "cd '$REMOTE_DIR' && mu script project-scripts local-db-copy '$BACKUP_NAME'"
step "   checking the backup exists" \
    ssh -i "$IDENTITY_FILE" "$REMOTE" "test -d '$BACKUP_DIR'"

echo "5. syncing data to $REMOTE_TARGET"
# Both backups come from the target, so rsync must not delete them.
sync_data "$LOCAL_DIR/data" "$REMOTE:$REMOTE_DIR/" \
    --exclude "/$BACKUP_NAME/" --exclude "/$USERS_BACKUP_NAME"

echo "6. removing copied users on the target"
step "   starting the triplestore" \
    remote "cd '$REMOTE_DIR' && docker compose up -d triplestore"
step_verbose "   waiting for the triplestore to accept queries" \
    wait_for_remote_triplestore
step_verbose "   mu script project-scripts data-sync restore-users $USERS_BACKUP_NAME" \
    remote_mu restore-users "$USERS_BACKUP_NAME"

echo "7. updating notification email on the target"
step_verbose "   mu script project-scripts data-sync update-notification-email $NOTIFICATION_EMAIL" \
    remote_mu update-notification-email "$NOTIFICATION_EMAIL"

echo "8. starting the stack on the target"
step "   docker compose up -d" \
    remote "cd '$REMOTE_DIR' && docker compose up -d"

echo ""
echo "## data sync to $REMOTE_TARGET completed."
echo ""
echo "The stack on $REMOTE_TARGET is running again. Verify it behaves as"
echo "expected and remove the backups once you are happy:"
echo "  ssh $REMOTE \"rm -rf '$BACKUP_DIR' '$USERS_BACKUP_DIR'\""
