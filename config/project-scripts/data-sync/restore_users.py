#!/usr/bin/env python3
"""Restore the users graph from a file, replacing its contents.

Usage:
  mu script project-scripts data-sync restore-users [input-file] [endpoint] [batch-size]

The input may be N-Quads or N-Triples. Quads for other graphs are ignored, and
N-Triples lines are assigned the users graph.

Destructive: the graph is cleared and replaced. No confirmation is asked and no
safety backup is written, so keep your own copy of anything you may need back.

After loading, login activities are set for users that have none.
"""

import os

from helpers import (
    DEFAULT_ENDPOINT,
    USERS_GRAPH as GRAPH,
    PROJECT_DIR,
    SparqlError,
    count_triples,
    normalise,
    query,
    warn,
)


def parse_args(argv):
    """Parse arguments; named flags are taken first, then positionals."""
    options = {
        "input": "users-graph.nq",
        "endpoint": DEFAULT_ENDPOINT,
        "batch_size": 250,
    }
    positional = []
    index = 0
    while index < len(argv):
        arg = argv[index]
        if arg in ("--endpoint", "--batch-size"):
            if index + 1 >= len(argv):
                raise ValueError(f"{arg} needs a value")
            index += 1
            if arg == "--endpoint":
                options["endpoint"] = argv[index]
            else:
                options["batch_size"] = int(argv[index])
        elif arg.startswith("-"):
            raise ValueError(f"unknown option {arg}")
        else:
            positional.append(arg)
        index += 1

    if len(positional) >= 1:
        options["input"] = positional[0]
    if len(positional) >= 2:
        options["endpoint"] = positional[1]
    if len(positional) >= 3:
        options["batch_size"] = int(positional[2])

    if options["batch_size"] < 1:
        raise ValueError("batch size must be at least 1")
    return options


def run(options):
    input_path = os.path.join(PROJECT_DIR, options["input"])
    endpoint = options["endpoint"]
    batch_size = options["batch_size"]

    if not os.path.isfile(input_path):
        warn(f"ERROR: input file {options['input']} not found.")
        return 1
    if os.path.getsize(input_path) == 0:
        warn(f"ERROR: input file {options['input']} is empty.")
        return 1

    with open(input_path, encoding="utf-8", errors="replace") as handle:
        raw_lines = handle.readlines()

    quads, stats = normalise(raw_lines, GRAPH)
    if stats["dropped"]:
        warn(f"WARNING: ignored {stats['dropped']} line(s) belonging to other graphs.")
    if stats["unparsed"]:
        warn(f"WARNING: skipped {stats['unparsed']} unparseable line(s).")

    total = len(quads)
    if total == 0:
        warn(f"ERROR: no quads for <{GRAPH}> found in {options['input']}.")
        return 1

    print(f"About to REPLACE the contents of <{GRAPH}> at {endpoint}")
    print(f"with {total} quads from {options['input']}  (batches of {batch_size})")
    print()

    print(f"Clearing <{GRAPH}>")
    query(endpoint, f"DELETE WHERE {{ GRAPH <{GRAPH}> {{ ?s ?p ?o }} }}", update=True)

    print(f"Inserting {total} quads")
    inserted = 0
    batch_number = 0
    for start in range(0, total, batch_size):
        chunk = quads[start:start + batch_size]
        suffix = f" <{GRAPH}> ."
        triples = " ".join(quad[:-len(suffix)] + " ." for quad in chunk)
        query(endpoint, f"INSERT DATA {{ GRAPH <{GRAPH}> {{ {triples} }} }}", update=True)

        inserted += len(chunk)
        batch_number += 1
        print(f"  batch {batch_number}: {inserted}/{total}")

    print()
    print(f"Restore finished. Sent {inserted} quads.")

    try:
        final = count_triples(endpoint, GRAPH)
        print(f"Graph now holds {final} triples.")
        if final != inserted:
            warn(
                f"WARNING: inserted {inserted} quads but the graph holds {final} "
                f"triples. Duplicate data may have existed, or the graph changed."
            )
    except SparqlError as error:
        warn(f"WARNING: could not verify the final triple count: {error}")

    ensure_login_activity(endpoint)

    return 0


def ensure_login_activity(endpoint):
    path = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                        "ensure-login-activity.sparql")
    if not os.path.isfile(path):
        warn(f"WARNING: {path} is missing, login activities were not set.")
        return

    with open(path, encoding="utf-8") as handle:
        sparql = handle.read()

    print("Setting login activity for users without one")
    try:
        query(endpoint, sparql, update=True)
    except SparqlError as error:
        warn(f"WARNING: setting login activities failed: {error}")
