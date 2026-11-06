#!/usr/bin/env python3
"""Back up the users graph to a file.

Usage:
  mu script project-scripts data-sync backup-users [output-file] [endpoint]

Defaults: users-graph.nq, http://triplestore:8890/sparql.
"""

import os

from helpers import (
    DEFAULT_ENDPOINT,
    USERS_GRAPH as GRAPH,
    PROJECT_DIR,
    SparqlError,
    as_term,
    count_triples,
    query,
    select,
    warn,
)

# SPARQL results only carry literals as values, so the datatype and language have
# to be asked for explicitly to be rendered again.
VARIABLES = "?s ?p ?o (DATATYPE(?o) AS ?dt) (LANG(?o) AS ?lang)"


def render(row):
    """Render one SELECT row as an N-Triples statement."""
    subject = as_term(row["s"])
    predicate = as_term(row["p"])
    term_type, value = row["o"]

    if term_type != "literal":
        return f"{subject} {predicate} {as_term(row['o'])} ."

    escaped = (
        value.replace("\\", "\\\\")
        .replace('"', '\\"')
        .replace("\n", "\\n")
        .replace("\r", "\\r")
        .replace("\t", "\\t")
    )
    literal = f'"{escaped}"'

    language = row["lang"][1]
    datatype = row["dt"][1]
    if language:
        literal += f"@{language}"
    elif datatype and datatype != "http://www.w3.org/2001/XMLSchema#string":
        literal += f"^^<{datatype}>"

    return f"{subject} {predicate} {literal} ."


def main(argv):
    """Back up the users graph. argv holds the arguments after the command."""
    for arg in argv:
        if arg.startswith("-"):
            raise ValueError(
                f"unknown option {arg!r}; the arguments are: "
                f"output-file, endpoint"
            )

    output = argv[0] if len(argv) >= 1 else "users-graph.nq"
    endpoint = argv[1] if len(argv) >= 2 else DEFAULT_ENDPOINT
    if len(argv) > 2:
        raise ValueError(f"unexpected argument {argv[2]!r}")

    output_path = os.path.join(PROJECT_DIR, output)
    if os.path.exists(output_path):
        warn(f"ERROR: {output_path} already exists. Remove it or pass another name.")
        return 1

    print(f"Backing up <{GRAPH}> from {endpoint}")

    total = count_triples(endpoint, GRAPH)
    print(f"Graph contains {total} triples")

    lines = []

    query_text = (
        f"SELECT {VARIABLES} WHERE {{ GRAPH <{GRAPH}> {{ ?s ?p ?o }} }} "
        f"ORDER BY ?s ?p ?o"
    )
    rows = select(endpoint, query_text)
    if not rows:
        warn(f"ERROR: no triples found in <{GRAPH}>.")
        return 1

    lines = [render(row) for row in rows]

    if total:
        percent = len(lines) * 100 // total
        print(f"Read {len(lines)}/{total} triples ({percent}%)")
    else:
        print(f"Read {len(lines)} triples")

    quads = [f"{line[:-2].rstrip()} <{GRAPH}> ." for line in lines]
    with open(output_path, "w", encoding="utf-8") as handle:
        handle.write("\n".join(quads) + "\n")

    print(f"Wrote {len(quads)} quads to {output_path}")

    if total and len(quads) != total:
        warn(
            f"WARNING: the server counted {total} triples but {len(quads)} were "
            f"backed up. The graph probably changed during the backup; re-run to get "
            f"a consistent snapshot."
        )
        return 1

    return 0
