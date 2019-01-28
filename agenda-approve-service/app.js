// VIRTUOSO bug: https://github.com/openlink/virtuoso-opensource/issues/515
import mu from 'mu';
import { ok } from 'assert';
import cors from 'cors';

const app = mu.app;
const bodyParser = require('body-parser');

app.use(cors())
app.use(bodyParser.json({ type: 'application/*+json' }))

app.post('/approveAgenda', async (req, res) => {
	let newAgendaId = req.body.newAgendaId;
	let oldAgendaId = req.body.oldAgendaId;

	let newAgendaURI = await getNewAgendaURI(newAgendaId);
	let agendaData = await copyAgendaItems(oldAgendaId, newAgendaURI)
	res.send({ status: ok, statusCode: 200, body: { agendaIds: { newAgendaId: newAgendaId, oldAgendaId: oldAgendaId }, agendaData: agendaData } });
});

async function getNewAgendaURI(newAgendaId) {
	let query = `
 PREFIX vo-besluit: <https://data.vlaanderen.be/ns/besluitvorming#>
 PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
 PREFIX vo-gen: <https://data.vlaanderen.be/ns/generiek#> 
 PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
 PREFIX prov-o: <http://www.w3.org/ns/prov#>

 SELECT ?agenda WHERE {
  GRAPH <http://mu.semte.ch/application> {
   ?agenda a vo-besluit:Agenda ;
   mu:uuid "${newAgendaId}" .
  }
 }
 `

	let data = await mu.query(query).catch(err => { console.error(err) });
	return data.results.bindings[0].agenda.value;

}

// ?p => Predicate
// ?o => Object
async function copyAgendaItems(oldId, newUri) {
	// SUBQUERY: Is needed to make sure the uuid isn't generated for every variable.
	const query = `

PREFIX vo-besluit: <https://data.vlaanderen.be/ns/besluitvorming#>
PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
PREFIX vo-gen: <https://data.vlaanderen.be/ns/generiek#> 
PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
PREFIX prov-o: <http://www.w3.org/ns/prov#>


INSERT { 
	GRAPH <http://mu.semte.ch/application> {
    <${newUri}> ext:agendapunt ?agendaitem .
    ?newURI ?p ?o .
    ?s ?p2 ?newURI .
    ?newURI mu:uuid ?newUuid
	} WHERE {
    { SELECT * { 
    		GRAPH <http://mu.semte.ch/application> {
  			?agenda a vo-besluit:Agenda ;
  			mu:uuid "${oldId}" .
  			?agenda ext:agendapunt ?agendaitem .
    
  			OPTIONAL { ?agendaitem mu:uuid ?olduuid } 
  			BIND(IF(BOUND(?olduuid), STRUUID(), STRUUID()) as ?uuid)
				BIND(IRI(CONCAT("http://localhost/vo/agendaitems/", ?uuid)) AS ?newURI) 

				{ SELECT ?agendaitem ?p ?o ?s ?p2 
					WHERE {
    				OPTIONAL {
      				?agendaitem ?p ?o .
    					FILTER(?p != mu:uuid)
    				}

    				OPTIONAL {
    					?s ?p2 ?agendaitem .
      				FILTER(?p2 != ext:agendapunt)
    				}
					}
				}    
			}
		} 
	}
	BIND(STRAFTER(STR(?newURI), "http://localhost/vo/agendaitems/") AS ?newUuid)
}`

	let data = await mu.query(query).catch(err => { console.error(err) });
	return data;
}

mu.app.use(mu.errorHandler);