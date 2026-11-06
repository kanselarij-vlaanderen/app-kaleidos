"""Shared helpers for the data-sync scripts."""

import json
import re
import sys
import urllib.error
import urllib.parse
import urllib.request

USERS_GRAPH = "http://mu.semte.ch/graphs/system/users"
EMAIL_GRAPH = "http://mu.semte.ch/graphs/system/email"
DEFAULT_ENDPOINT = "http://triplestore:8890/sparql"
PROJECT_DIR = "/project"

JSON_RESULTS = "application/sparql-results+json"
NTRIPLES = "application/n-triples"

# One RDF term: IRI, blank node, or literal with optional language tag or
# datatype.
_TERM = re.compile(
    r"""
      <[^>]*>                                # IRI
    | _:[^\s]+                               # blank node
    | "(?:[^"\\]|\\.)*"                      # literal body
      (?:@[A-Za-z0-9-]+|\^\^<[^>]*>)?        # optional language / datatype
    """,
    re.VERBOSE,
)


class SparqlError(RuntimeError):
    pass


def query(endpoint, sparql, accept=JSON_RESULTS, timeout=600, update=False):
    """Run a SPARQL query and return the decoded body.

    Updates and long queries are sent as a POST body: a GET puts the whole
    query in the URL, which Virtuoso rejects once it grows beyond a few
    kilobytes, and write requests do not belong in a query string.
    """
    body = urllib.parse.urlencode({"query": sparql}).encode("utf-8")
    headers = {"Accept": accept, "Content-Type": "application/x-www-form-urlencoded"}

    if update or len(body) > 2000:
        request = urllib.request.Request(endpoint, data=body, headers=headers)
    else:
        request = urllib.request.Request(endpoint + "?" + body.decode("utf-8"),
                                         headers=headers)

    try:
        with urllib.request.urlopen(request, timeout=timeout) as response:
            return response.read().decode("utf-8")
    except urllib.error.HTTPError as exc:
        detail = exc.read().decode("utf-8", "replace").strip()
        raise SparqlError(f"HTTP {exc.code} from {endpoint}: {detail[:500]}") from exc
    except urllib.error.URLError as exc:
        raise SparqlError(f"cannot reach {endpoint}: {exc.reason}") from exc


def select(endpoint, sparql):
    """Run a SELECT and return the list of rows as dicts.

    Each value is a (type, value) pair, e.g. ("uri", "http://sample.org/a") or
    ("bnode", "b0"), so callers can render the term correctly.
    """
    body = query(endpoint, sparql, JSON_RESULTS)
    payload = json.loads(body)
    return [
        {name: (binding["type"], binding["value"]) for name, binding in row.items()}
        for row in payload.get("results", {}).get("bindings", [])
    ]


def as_term(term):
    """Render a (type, value) pair as SPARQL syntax.

    IRIs are wrapped in angle brackets; blank nodes keep their "_:" prefix and
    must not be bracketed, or they would be read as an IRI with that name.
    """
    term_type, value = term
    if term_type == "bnode":
        return value if value.startswith("_:") else f"_:{value}"
    return f"<{value}>"


def count_triples(endpoint, graph=USERS_GRAPH):
    """Return the number of triples in the graph."""
    rows = select(
        endpoint,
        f"SELECT (COUNT(*) AS ?c) WHERE {{ GRAPH <{graph}> {{ ?s ?p ?o }} }}",
    )
    return int(rows[0]["c"][1]) if rows else 0


def ntriples_to_nquads(body, graph=USERS_GRAPH):
    """Assign a graph to N-Triples output, producing N-Quads lines."""
    lines = []
    for line in body.splitlines():
        line = line.strip()
        if not line:
            continue
        if not line.endswith(" ."):
            raise SparqlError(f"unexpected N-Triples line: {line[:200]}")
        lines.append(f"{line[:-2].rstrip()} <{graph}> .")
    return lines


def split_terms(line):
    """Split a line into RDF terms, or return None when it does not parse.

    Terms are counted, not matched by line ending: an N-Triples line may end in
    an IRI object, which looks exactly like a trailing graph term.
    """
    terms = []
    pos = 0
    end = len(line)
    while True:
        while pos < end and line[pos].isspace():
            pos += 1
        if pos >= end:
            break
        if line[pos] == "." and not line[pos + 1:].strip():
            return terms
        match = _TERM.match(line, pos)
        if not match:
            return None
        terms.append(match.group(0))
        pos = match.end()
    return terms


def normalise(lines, graph=USERS_GRAPH):
    """Turn mixed N-Quads/N-Triples lines into N-Quads for one graph.

    Returns (kept_lines, stats).
    """
    target = f"<{graph}>"
    kept = []
    stats = {"kept": 0, "converted": 0, "dropped": 0, "unparsed": 0}

    for line in lines:
        line = line.strip()
        if not line or line.startswith("#"):
            continue
        terms = split_terms(line)
        if terms is None or len(terms) not in (3, 4):
            stats["unparsed"] += 1
            continue
        if len(terms) == 4:
            if terms[3] != target:
                stats["dropped"] += 1
                continue
            stats["kept"] += 1
            kept.append(" ".join(terms) + " .")
        else:
            stats["converted"] += 1
            kept.append(" ".join(terms) + f" {target} .")
    return kept, stats


def warn(message):
    print(message, file=sys.stderr, flush=True)


def enable_line_buffering():
    """Flush output line by line instead of at exit.

    Output is collected by the caller rather than shown in a terminal, so
    stdout is block-buffered and progress appears all at once when the script
    ends.
    """
    for stream in (sys.stdout, sys.stderr):
        try:
            stream.reconfigure(line_buffering=True)
        except AttributeError:
            pass
