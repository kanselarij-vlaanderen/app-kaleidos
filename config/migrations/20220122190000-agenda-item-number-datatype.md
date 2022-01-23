# Migrating a mixed datatype

Write the triple you need to a temporary predicate

```sparql
PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

INSERT {
    GRAPH ?g {
        ?item ext:temp_prioriteit ?prio_int .
    }
}
WHERE{
    GRAPH ?g {
        ?item a besluit:Agendapunt ;
            ext:prioriteit ?prio .
        FILTER(datatype(?prio) != xsd:integer)
        BIND(STRDT(STR(?prio), xsd:integer)AS ?prio_int)
    }
}
```

Delete triples containing the wrong datatype

```sparql
PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

DELETE {
    GRAPH ?g {
        ?item ext:prioriteit ?prio .
    }
}
WHERE {
    GRAPH ?g {
        ?item a besluit:Agendapunt ;
            ext:prioriteit ?prio ;
            ext:temp_prioriteit ?tmp_prio .
    }
}
```



For each graph in the following list, repeat below steps:

```
<http://mu.semte.ch/graphs/organizations/kanselarij>
<http://mu.semte.ch/graphs/organizations/intern-overheid>
<http://mu.semte.ch/graphs/organizations/minister>
<http://mu.semte.ch/graphs/organizations/intern-regering>
```

Dump wanted triples (with correct datatype) to a ttl file by means of a construct (select pretty Turtle)
```sparql
PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

CONSTRUCT {
    ?item ext:prioriteit ?tmp_prio .
}
WHERE {
    GRAPH <http://mu.semte.ch/graphs/organizations/kanselarij> {
        ?item a besluit:Agendapunt ;
            ext:temp_prioriteit ?tmp_prio .
    }
}
```

Create a migration (`ttl`-file + `graph` file) inserting the dumped triples.

Create a migration to remove the temporary triples
```sparql
DELETE {
  GRAPH ?g {
    ?s <http://mu.semte.ch/vocabularies/ext/temp_prioriteit> ?o .
  }
} WHERE {
  GRAPH ?g {
    ?s <http://mu.semte.ch/vocabularies/ext/temp_prioriteit> ?o .
  }
}
```
