import flask
import os
import helpers
import builtins
from escape_helpers import sparql_escape
from rdflib.namespace import Namespace



@app.route("/transfer/procedures", methods=['POST'])
def add_mandate_to_open_subcases():
    req = flask.request.json
    old_mandatee = req['old_mandatee']
    open_cases = select_distict_open_subcases_for_mandate(old_mandatee)
    new_mandatee = req['new_mandatee']
    for subcase in open_cases:
        q = f"""
            PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>

            INSERT DATA {{
                GRAPH <http://mu.semte.ch/application> {{
                    <{subcase}> besluitvorming:heeftBevoegde <{new_mandatee}> .
                }}
            }}
        """
        helpers.query(q)
    return flask.jsonify(open_cases)


def select_distict_open_subcases_for_mandate(old_mandatee):
    q = f"""
        PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
        PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
        PREFIX dct: <http://purl.org/dc/terms/>
        PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

        SELECT DISTINCT ?subcase
        WHERE {{
           GRAPH <http://mu.semte.ch/application> {{
              ?mandatee a mandaat:Mandataris .
              FILTER(?mandatee = <{old_mandatee}>)
              ?mandatee mandaat:start ?startdate .
              ?mandatee mandaat:einde ?enddate .
              ?mandatee mandaat:beleidsdomein ?mandateedomain .

              ?case a dbpedia:Case .
              ?case dct:hasPart ?subcase .
              ?subcase mandaat:beleidsdomein ?domain .
              ?subcase dct:created ?created .
              FILTER (?created > ?startdate)
              FILTER (?created < ?enddate)
              FILTER (?mandateedomain = ?domain)
              OPTIONAL {{
                   ?subcase besluitvorming:isGeagendeerdVia ?agenda .
                   OPTIONAL {{
                        ?agenda ext:accepted ?accepted .
                   }}
              }}
              FILTER (! BOUND(?accepted) || STR(?accepted) != "true")
           }}
        }}
    """
    data = helpers.query(q)['results']['bindings']
    return [x['subcase']['value'] for x in data]



