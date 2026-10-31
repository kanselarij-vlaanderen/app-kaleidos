#!/usr/bin/env python3
"""Manage data that has to be kept in sync across environments.

Usage:
  mu script project-scripts data-sync backup-users [output-file] [endpoint]
  mu script project-scripts data-sync restore-users [input-file] [endpoint] [batch-size]
  mu script project-scripts data-sync update-notification-email <email-address> [endpoint]
"""

import sys

import backup_users
import restore_users
import update_notification_email

from helpers import SparqlError, enable_line_buffering, warn


def usage(warn):
    warn("Usage: data-sync <command> [arguments...]")
    warn("")
    warn("  backup-users [output-file] [endpoint]")
    warn("      Write the users graph to a file. Default: users-graph.nq.")
    warn("")
    warn("  restore-users [input-file] [endpoint] [batch-size]")
    warn("      Replace the users graph from a file. Default: users-graph.nq,")
    warn("      250 quads per batch.")
    warn("")
    warn("  update-notification-email <email-address> [endpoint]")
    warn("      Set every notification address to <email-address>.")


def main(argv):
    if not argv or argv[0] in ("-h", "--help", "help"):
        usage(warn)
        return 1 if not argv else 0

    command = argv[0]
    arguments = argv[1:]

    if command == "backup-users":
        return backup_users.main(arguments)
    if command == "restore-users":
        return restore_users.run(restore_users.parse_args(arguments))
    if command == "update-notification-email":
        return update_notification_email.main(arguments)

    warn(f"ERROR: unknown command {command!r}.")
    usage(warn)
    return 1


if __name__ == "__main__":
    enable_line_buffering()
    print()
    try:
        sys.exit(main(sys.argv[1:]))
    except SparqlError as error:
        warn(f"ERROR: {error}")
        sys.exit(1)
    except ValueError as error:
        warn(f"ERROR: {error}")
        sys.exit(1)
