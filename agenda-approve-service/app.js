// VIRTUOSO bug: https://github.com/openlink/virtuoso-opensource/issues/515
// This is why we are looping over every agendaitem to add it to a new agenda.
import mu from 'mu';
import { ok } from 'assert';
import cors from 'cors';

const app = mu.app;
const bodyParser = require('body-parser');

app.use(cors())
app.use(bodyParser.json({ type: 'application/*+json' }))

app.post('/approveAgenda', async (req, res) => {
	console.log(req.body)
	let newAgendaId = req.body.newAgendaId;
	let oldAgendaId = req.body.oldAgendaId;

	// let agendaData = await getAgendaWithRelations(oldAgendaId, newAgendaId);
	res.send({ status: ok, statusCode: 200, body: { agendaIds: { newAgendaId: newAgendaId, oldAgendaId: oldAgendaId } } });
});

async function createNewAgenda() {

}

// ?p => Predicate
// ?o => Object

async function copyAgendaItems(id) {
	const query = `
  PREFIX vo-besluit: <https://data.vlaanderen.be/ns/besluitvorming#>
  PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
  PREFIX vo-gen: <https://data.vlaanderen.be/ns/generiek#> 
	PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
	PREFIX prov-o: <http://www.w3.org/ns/prov#>

	INSERT {
    GRAPH <http://mu.semte.ch/application> {
		?agenda ext:agendapunt ?agendaitem .
		?session vo-besluit:Zitting ?sess .
    ?newURI ?p ?o .
		?newURI mu:uuid ?uuid.
    ?s ?p2 ?newURI .
  	}
	} WHERE {
    GRAPH <http://mu.semte.ch/application> 
    {
		?agenda a vo-besluit:Agenda ;
							mu:uuid "${id}" ;
							ext:goedgekeurd ?locked .
		?agenda ext:agendapunt ?agendaitem .
		BIND(STRUUID() AS ?uuid)
		BIND(IRI(CONCAT("http://localhost/vo/agendaitems/", ?uuid)) AS ?newURI) 
		OPTIONAL {
    	?agendaitem ?p ?o .
			FILTER(?p != mu:uuid)
		}
    OPTIONAL {
      ?s ?p2 ?agendaitem .
		}
		?session vo-besluit:Zitting ?sess .
	}
}`

	let data = await mu.query(query).catch(err => { console.log(err) });
	return data;
}

mu.app.use(mu.errorHandler);


/**
INSERT DATA {
	GRAPH <http://mu.semte.ch/application>
{
<http://localhost/vo/agendaas> a vo-besluit:Agenda ;
 ext:naam """Ontwerpagenda""".
}
}

PREFIX vo-besluit: <https://data.vlaanderen.be/ns/besluitvorming#>
PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
PREFIX vo-gen: <https://data.vlaanderen.be/ns/generiek#> 
PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
PREFIX prov-o: <http://www.w3.org/ns/prov#>

CONSTRUCT {
    $agendaURI ext:agendapunt ?agendaitem .
    ?newURI ?p ?o .
    ?s ?p2 ?newURI .
    ?session vo-besluit:Zitting ?sess .
} WHERE {
    GRAPH <http://mu.semte.ch/application> 
{
        ?agenda a vo-besluit:Agenda ;
        mu:uuid "5C480F5C798D10000C000033" ;
	ext:goedgekeurd ?locked .
        ?agenda ext:agendapunt ?agendaitem .
        
        OPTIONAL { ?agendaitem mu:uuid ?olduuid } 
        BIND(IF(BOUND(?olduuid), STRUUID(), STRUUID()) as ?uuid)
        BIND(IRI(CONCAT("http://localhost/vo/agendaitems/", ?uuid)) AS ?newURI) 

	OPTIONAL {
    	?agendaitem ?p ?o .
	FILTER(?p != mu:uuid)
        }
        OPTIONAL {
        ?s ?p2 ?agendaitem .
        }

}
}
  */