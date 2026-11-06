#!/usr/bin/env python3
"""Set the notification email addresses used by the application.

Usage:
  mu script project-scripts data-sync update-notification-email <email-address> [endpoint]
"""

from helpers import (
    EMAIL_GRAPH as GRAPH,
    DEFAULT_ENDPOINT,
    SparqlError,
    query,
    select,
    warn,
)

PREFIX = "http://mu.semte.ch/vocabularies/ext/"

# The addresses shown on the settings page, excluding ext:defaultFromEmail.
PREDICATES = [
    "ext:translationRequestToEmail",
    "ext:translationRequestCcEmail",
    "ext:translationRequestReplyToEmail",
    "ext:proofRequestToEmail",
    "ext:proofRequestCcEmail",
    "ext:proofRequestReplyToEmail",
    "ext:publicationRequestToEmail",
    "ext:publicationRequestCcEmail",
    "ext:publicationRequestReplyToEmail",
    "ext:cabinetSubmissionsSecretaryEmail",
    "ext:cabinetSubmissionsIKWEmail",
    "ext:cabinetSubmissionsIKWConfidentialEmail",
    "ext:cabinetSubmissionsReplyToEmail",
]

RESOURCE_TYPE = f"{PREFIX}EmailNotificationSettings"


def usage(warn):
    warn("Usage: update-notification-email <email-address> [endpoint]")
    warn("")
    warn("  Sets every notification address to <email-address>.")


def validate(email):
    """Reject anything that is not a plausible email address."""
    if email.count("@") != 1:
        raise ValueError(f"{email!r} is not an email address")
    local, domain = email.split("@")
    if not local or not domain or "." not in domain:
        raise ValueError(f"{email!r} is not an email address")
    if any(character.isspace() for character in email):
        raise ValueError(f"{email!r} contains whitespace")
    return email


def escape_literal(email):
    """Escape a string for use inside a SPARQL double-quoted literal."""
    return email.replace("\\", "\\\\").replace('"', '\\"')


def main(argv):
    if not argv or argv[0] in ("-h", "--help", "help"):
        usage(warn)
        return 1 if not argv else 0

    email = validate(argv[0])
    if len(argv) > 2:
        raise ValueError(f"unexpected argument {argv[2]!r}")
    endpoint = argv[1] if len(argv) > 1 else DEFAULT_ENDPOINT

    print(f"Setting notification addresses to {email}")

    settings = select(
        endpoint,
        f"SELECT DISTINCT ?settings WHERE {{ GRAPH <{GRAPH}> "
        f"{{ ?settings a <{RESOURCE_TYPE}> }} }}",
    )
    if not settings:
        warn(f"ERROR: no <{RESOURCE_TYPE}> found in <{GRAPH}>.")
        return 1

    print(f"Updating {len(settings)} settings resource(s), {len(PREDICATES)} addresses each")

    literal = escape_literal(email)
    for settings_row in settings:
        settings_iri = settings_row["settings"][1]
        for predicate in PREDICATES:
            query(
                endpoint,
                f"PREFIX ext: <{PREFIX}>\n"
                f"DELETE {{ GRAPH <{GRAPH}> {{ ?settings {predicate} ?old . }} }}\n"
                f"WHERE {{ GRAPH <{GRAPH}> {{ ?settings {predicate} ?old . }} }}",
                update=True,
            )
            query(
                endpoint,
                f"PREFIX ext: <{PREFIX}>\n"
                f"INSERT DATA {{ GRAPH <{GRAPH}> {{ "
                f"<{settings_iri}> {predicate} \"{literal}\" . }} }}",
                update=True,
            )

    print(f"Done. Every notification address is now {email}")
    return 0
