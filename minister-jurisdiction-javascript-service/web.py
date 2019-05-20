import flask
import os
import helpers
import builtins
from escape_helpers import sparql_escape
from rdflib.namespace import Namespace

app = flask.Flask(__name__)


@app.route("/", methods=['GET'])
def home():
    return "Server is up and running"


def parse_list_of_nested_dicts(input_list):
    parsed_data = list()
    for element in input_list:
        parsed_element = dict()
        for key in element.keys():
            parsed_element[key] = element[key]['value']
        parsed_data.append(parsed_element)
    return parsed_data


@app.route('/templateExample/', methods=['GET'])
def query():
    """Example query: Returns all the triples in the application graph in a JSON
    format."""
    q = """ 
        SELECT *
        WHERE {
            GRAPH <http://mu.semte.ch/application> {
                ?s ?p ?o
            }
        }
    """
    return flask.jsonify(helpers.query(q))


@app.route('/ministers/', methods=['GET'])
def get_ministers():
    q = """ 
            PREFIX core: <http://mu.semte.ch/vocabularies/core/>
            PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
            PREFIX persoon: <http://data.vlaanderen.be/ns/persoon#>
            PREFIX dct: <http://purl.org/dc/terms/>
            PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
            PREFIX foaf: <http://xmlns.com/foaf/0.1/>
            PREFIX adms: <http://www.w3.org/ns/adms#>
            PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

            SELECT *
            WHERE {
                GRAPH <http://mu.semte.ch/application> {
                    ?mandatee a mandaat:Mandataris ;
                    dct:title ?title ;
                    mandaat:isBestuurlijkeAliasVan ?person .
                    ?person foaf:familyName ?familyName ;
                    foaf:firstName ?firstName             
                }
            }
        """
    data = helpers.query(q)
    for item in data['results']['bindings']:
        print(f"{item['firstName']['value']} {item['familyName']['value']} is {item['title']['value']}")
    return flask.jsonify(data['results']['bindings'])


@app.route("/domains/ministers")
def get_ministers_for_domains():
    q = """
        PREFIX core: <http://mu.semte.ch/vocabularies/core/>
        PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
        PREFIX persoon: <http://data.vlaanderen.be/ns/persoon#>
        PREFIX dct: <http://purl.org/dc/terms/>
        PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        PREFIX adms: <http://www.w3.org/ns/adms#>
        PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
        PREFIX org: <http://www.w3.org/ns/org#>
        
        SELECT *
        WHERE {
            GRAPH <http://mu.semte.ch/application> {
                ?domain a ext:BeleidsdomeinCode ;
                skos:prefLabel ?label ;
                skos:scopeNote ?note .
                ?mandatee mandaat:beleidsdomein ?domain .
                ?mandatee mandaat:isBestuurlijkeAliasVan ?person .
                ?person foaf:familyName ?familyName ;
                foaf:firstName ?firstName
            }
        }
    """
    data = helpers.query(q)['results']['bindings']
    return flask.jsonify(parse_list_of_nested_dicts(data))


@app.route("/domain/mandatee", methods=['POST'])
def get_mandatee_for_domain():
    req = flask.request.json
    domain = req['domain']
    q = f"""
        PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        
        SELECT *
        WHERE {{
            GRAPH <http://mu.semte.ch/application> {{
                ?mandatee mandaat:beleidsdomein <{domain}> .
                ?mandatee mandaat:isBestuurlijkeAliasVan ?person .
                ?person foaf:familyName ?familyName ;
                foaf:firstName ?firstName
            }}
        }}
    """
    return flask.jsonify(helpers.query(q))


@app.route("/mandatee/domains", methods=['POST'])
def get_domains_for_mandatee():
    req = flask.request.json
    mandatee = req['mandatee']
    q = f"""
    PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    
    SELECT *
    WHERE {{
        GRAPH <http://mu.semte.ch/application> {{
            <{mandatee}> mandaat:beleidsdomein ?domain .
            ?domain skos:prefLabel ?label
        }}
    }}
    """
    return flask.jsonify(helpers.query(q))


@app.route("/domain/transfer", methods=['POST'])
def transfer_domain():
    req = flask.request.json
    receiving_mandatee = req['receiving_mandatee']
    domain = req['transferred_domain']
    q = f"""
        PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
        PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
            
        DELETE WHERE {{
            GRAPH <http://mu.semte.ch/application> {{
                ?mandatee mandaat:beleidsdomein <{domain}> .            
            }}
        }}
        
        INSERT DATA {{ 
            GRAPH <http://mu.semte.ch/application> {{ 
                <{receiving_mandatee}> mandaat:beleidsdomein <{domain}> .
            }}
        }}
    """
    return flask.jsonify(helpers.query(q))


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


"""
Vocabularies
"""
mu = Namespace('http://mu.semte.ch/vocabularies/')
mu_core = Namespace('http://mu.semte.ch/vocabularies/core/')
mu_ext = Namespace('http://mu.semte.ch/vocabularies/ext/')

graph = os.environ.get('MU_APPLICATION_GRAPH')
SERVICE_RESOURCE_BASE = 'http://mu.semte.ch/services/'

"""
Start Application
"""
if __name__ == '__main__':
    builtins.app = app
    builtins.helpers = helpers
    builtins.sparql_escape = sparql_escape
    app_file = os.environ.get('APP_ENTRYPOINT')
    f = open('/app/__init__.py', 'w+')
    f.close()
    try:
        exec("from ext.app.%s import *" % app_file)
    except Exception as e:
        helpers.log(str(e))
    debug = True if (os.environ.get('MODE') == "development") else False
    app.run(debug=debug, host='0.0.0.0', port=8088)
