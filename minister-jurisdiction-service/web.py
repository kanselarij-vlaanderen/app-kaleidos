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
    return flask.jsonify(data)


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
    app.run(debug=debug, host='0.0.0.0', port=8089)
