// VIRTUOSO bug: https://github.com/openlink/virtuoso-opensource/issues/515
import mu from 'mu';
import { ok } from 'assert';
import cors from 'cors';
const uuidv4 = require('uuid/v4');
const targetGraph = "http://mu.semte.ch/graphs/organizations/kanselarij";

const app = mu.app;
const moment = require('moment');
const bodyParser = require('body-parser');

app.use(cors())
app.use(bodyParser.json({ type: 'application/*+json' }))

app.post('/approveAgenda', async (req, res) => {
	const newAgendaId = req.body.newAgendaId;
	const oldAgendaId = req.body.oldAgendaId;

	const newAgendaURI = await getNewAgendaURI(newAgendaId);
	const agendaData = await copyAgendaItems(oldAgendaId, newAgendaURI);

	await markAgendaItemsPartOfAgendaA(oldAgendaId);
	await storeAgendaItemNumbers(oldAgendaId);
	await nameDocumentsBasedOnAgenda(oldAgendaId);

	try {
		const codeURI = await getSubcasePhaseCode();
		const subcasePhasesOfAgenda = await getSubcasePhasesOfAgenda(newAgendaId, codeURI);

		await checkForPhasesAndAssignMissingPhases(subcasePhasesOfAgenda, codeURI);
	} catch (e) {
		console.log("something went wrong while assigning the code 'Geagendeerd' to the agendaitems", e);
	}

	res.send({ status: ok, statusCode: 200, body: { agendaData: agendaData } }); // resultsOfSerialNumbers: resultsAfterUpdates
});

async function getSubcasePhaseCode() {
	const query = `
	PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
	PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
	PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
	PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
	PREFIX dbpedia: <http://dbpedia.org/ontology/>
	PREFIX dct: <http://purl.org/dc/terms/>
	PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
	
	SELECT ?code WHERE {
		GRAPH <${targetGraph}> {
					?code a ext:ProcedurestapFaseCode ;
                  skos:prefLabel ?label .
           				FILTER(UCASE(?label) = UCASE("geagendeerd"))  
		}
	}
`
	const data = await mu.query(query).catch(err => { console.error(err) });
	console.log(data);
	return data.results.bindings[0].code.value;
}

async function getSubcasePhasesOfAgenda(newAgendaId, codeURI) {
	const query = `
	PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
	PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
	PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
	PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
	PREFIX dbpedia: <http://dbpedia.org/ontology/>
	PREFIX dct: <http://purl.org/dc/terms/>
	PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
	
	SELECT ?agenda ?agendaitem ?subcase ?phases WHERE {
		GRAPH <${targetGraph}> {
				?agenda a besluitvorming:Agenda ;
									mu:uuid "${newAgendaId}" .
				?agenda   dct:hasPart ?agendaitem .
				?subcase  besluitvorming:isGeagendeerdVia ?agendaitem .
				OPTIONAL{ 
					        ?subcase ext:subcaseProcedurestapFase ?phases . 
									?phases  ext:procedurestapFaseCode <${codeURI}> . 
								}	 
		}
	}
`
	const data = await mu.query(query).catch(err => { console.error(err) });
	return data;
}

async function markAgendaItemsPartOfAgendaA(agendaId) {
	const query = `
	PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
	PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
	PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
	PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
	PREFIX dbpedia: <http://dbpedia.org/ontology/>
	PREFIX dct: <http://purl.org/dc/terms/>
	PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
	
	INSERT {
		GRAPH <${targetGraph}> {
			?agendaItem ext:partOfFirstAgenda """true"""^^<http://mu.semte.ch/vocabularies/typed-literals/boolean> .
		}
	} WHERE {
		GRAPH <${targetGraph}> {
			?agenda a besluitvorming:Agenda .
			?agenda mu:uuid "${agendaId}" .
			FILTER NOT EXISTS {
				?agenda besluitvorming:heeftVorigeVersie ?o.
			}
			?agenda dct:hasPart ?agendaItem .
		}
	}`;
	return await mu.query(query).catch(err => { console.error(err) });
};

async function storeAgendaItemNumbers(agendaId) {
	const maxAgendaItemNumberSoFar = await getHighestAgendaItemNumber(agendaId);
	let query = `PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
	PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
	PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
	PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
	PREFIX dbpedia: <http://dbpedia.org/ontology/>
	PREFIX dct: <http://purl.org/dc/terms/>
	PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
	
	SELECT ?agendaItem WHERE {
		GRAPH <${targetGraph}> {
			?agenda a besluitvorming:Agenda .
			?agenda mu:uuid "${agendaId}".
			?agenda dct:hasPart ?agendaItem .
			OPTIONAL {
				?agendaItem ext:prioriteit ?priority .
			}
			BIND(IF(BOUND(?priority), ?priority, 1000000) AS ?priorityOrMax)
			FILTER NOT EXISTS {
				?agendaItem ext:agendaItemNumber ?number .
			}
		}
	} ORDER BY ?priorityOrMax`;
	const sortedAgendaItemsToName = await mu.query(query).catch(err => { console.error(err) });
	const triples = [];
	sortedAgendaItemsToName.results.bindings.map((binding, index) => {
		triples.push(`<${binding['agendaItem'].value}> ext:agendaItemNumber ${maxAgendaItemNumberSoFar + index} .`);
	});

	query = `PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
	PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
	PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
	PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
	PREFIX dbpedia: <http://dbpedia.org/ontology/>
	PREFIX dct: <http://purl.org/dc/terms/>
	PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
	
	INSERT DATA {
		GRAPH <${targetGraph}> {
			${triples.join("\n")}
		}
	}`;
	await mu.query(query).catch(err => { console.log(err); })
};

async function getHighestAgendaItemNumber(agendaId) {
	const query = `PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
	PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
	PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
	PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
	PREFIX dbpedia: <http://dbpedia.org/ontology/>
	PREFIX dct: <http://purl.org/dc/terms/>
	PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
	
	SELECT MAX(?number) as ?max WHERE {
		GRAPH <${targetGraph}> {
			?agenda a besluitvorming:Agenda .
			?agenda mu:uuid "${agendaId}" .			
			?agenda dct:hasPart ?agendaItem .
			?agendaItem ext:agendaItemNumber ?number .
		}
	} GROUP BY ?agenda`;
	const response = await mu.query(query).catch(err => { console.error(err) });
	return parseInt(((response.results.bindings[0] || {})['max'] || {}).value || 0);
};

async function nameDocumentsBasedOnAgenda(agendaId) {
	const query = `PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
	PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
	PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
	PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
	PREFIX dbpedia: <http://dbpedia.org/ontology/>
	PREFIX dct: <http://purl.org/dc/terms/>
	PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
	
	SELECT ?agendaItem ?existingNumbers ?document ?number ?zittingDate ?dossierType WHERE {
		GRAPH <${targetGraph}> {
			?agenda a besluitvorming:Agenda .
			?agenda mu:uuid "${agendaId}" .
			?agenda besluit:isAangemaaktVoor ?zitting .
			?zitting besluit:geplandeStart ?zittingDate .
			?agenda dct:hasPart ?agendaItem .
			?agendaItem ext:bevatAgendapuntDocumentversie ?documentVersion .
			?subcase besluitvorming:isGeagendeerdVia ?agendaItem .
			OPTIONAL {
			  ?case dct:hasPart ?subcase .
				?case dct:type ?dossierType .
			}
			?document besluitvorming:heeftVersie ?documentVersion .
			FILTER NOT EXISTS {
				?document besluitvorming:stuknummerVR ?vrnumber .
			}
			{ SELECT ?agendaItem COUNT(DISTINCT(?othervrnumber)) AS ?existingNumbers WHERE {
				GRAPH <${targetGraph}> {
					?agendaItem ext:bevatAgendapuntDocumentversie ?otherVersion .
					?otherDocument besluitvorming:heeftVersie ?otherVersion .
					OPTIONAL { ?otherDocument besluitvorming:stuknummerVR ?othervrnumber . }
				}
			} }
			
			?agendaItem ext:agendaItemNumber ?number.
		}
	} ORDER BY ?agendaItem
	`;

	const response = await mu.query(query).catch(err => { console.error(err) });
	let previousAgendaItem = null;
	let previousStartingIndex = 0;
	let triples = [];
	response.results.bindings.map((binding) => {
		let item = binding['agendaItem'].value;
		let numbersSoFar = parseInt(binding['existingNumbers'].value) || 0;
		let document = binding['document'].value;
		let number = parseInt(binding['number'].value);
		let date = moment(binding['zittingDate'].value);
<<<<<<< HEAD
		let type = binding['dossierType'].value.indexOf("5fdf65f3-0732-4a36-b11c-c69b938c6626") > 0 ? "MED" : "DOC";
=======
		let type = (((binding['dossierType'] || {}).value) || "").indexOf("5fdf65f3-0732-4a36-b11c-c69b938c6626") > 0 ? "MED": "DOC";
>>>>>>> c5e77d90f17e147cce85bf083768c0577a231a3e

		if (previousAgendaItem != item) {
			previousAgendaItem = item;
			previousStartingIndex = numbersSoFar;
		}
		previousStartingIndex = previousStartingIndex + 1;
		number = paddNumberWithZeros(number, 4);
		let month = paddNumberWithZeros(date.month(), 2);
		let day = paddNumberWithZeros(date.date(), 2);
		triples.push(`<${document}> besluitvorming:stuknummerVR "VR ${date.year()} ${month}${day} ${type}.${number}/${previousStartingIndex}" .`);
	});

	if (triples.length < 1) {
		return;
	}

	await mu.query(`PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
	PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
	PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
	PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
	PREFIX dbpedia: <http://dbpedia.org/ontology/>
	PREFIX dct: <http://purl.org/dc/terms/>
	PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
		
	INSERT DATA {
	 GRAPH <${targetGraph}> {
					${triples.join("\n")}
	 }
	};`).catch(err => { console.error(err); });
};

function paddNumberWithZeros(number, length) {
	let string = "" + number;
	while (string.length < length) {
		string = 0 + string;
	}
	return string;
}

async function checkForPhasesAndAssignMissingPhases(subcasePhasesOfAgenda, codeURI) {
	if (subcasePhasesOfAgenda) {
		const parsedObjects = parseSparqlResults(subcasePhasesOfAgenda);

		const uniqueSubcaseIds = [...new Set(parsedObjects.map((item) => item['subcase']))];
		console.log(uniqueSubcaseIds)
		let subcaseListOfURIS = [];

		await uniqueSubcaseIds.map((id) => {
			const foundObject = parsedObjects.find((item) => item.subcase === id);
			if (foundObject && foundObject.subcase && !foundObject.phases) {
				subcaseListOfURIS.push(foundObject.subcase);
			}
			return id;
		});
		return await createNewSubcasesPhase(codeURI, subcaseListOfURIS)
	}
}

async function createNewSubcasesPhase(codeURI, subcaseListOfURIS) {
	const listOfQueries = await subcaseListOfURIS.map((subcaseURI) => {
		const newUUID = uuidv4();
		const newURI = `http://data.vlaanderen.be/id/ProcedurestapFase/${newUUID}`;
		console.log(codeURI)
		return `
		<${newURI}> a 	ext:ProcedurestapFase ;
		mu:uuid "${newUUID}" ;
		besluitvorming:statusdatum """${new Date().toISOString()}"""^^xsd:dateTime ;
		ext:procedurestapFaseCode <${codeURI}> .
		<${subcaseURI}> ext:subcaseProcedurestapFase <${newURI}> .
		`
	})

	const insertString = listOfQueries.join(' ');
	console.log(insertString);
	const query = `
	PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
	PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
	PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
	PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
	PREFIX dbpedia: <http://dbpedia.org/ontology/>
	PREFIX dct: <http://purl.org/dc/terms/>
	PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
		
	INSERT DATA {
	 GRAPH <${targetGraph}> {
					${insertString}
	 }
	};
`

	return await mu.update(query).catch(err => { console.error(err) });
}

async function getNewAgendaURI(newAgendaId) {
	const query = `
 	PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
 	PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
 	PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

 	SELECT ?agenda WHERE {
  	GRAPH <${targetGraph}> {
   		?agenda a besluitvorming:Agenda ;
   		mu:uuid "${newAgendaId}" .
  	}
 	}
 `

	const data = await mu.query(query).catch(err => { console.error(err) });
	return data.results.bindings[0].agenda.value;
}

async function copyAgendaItems(oldId, newUri) {
	// SUBQUERY: Is needed to make sure the uuid isn't generated for every variable.
	const createNewUris = `
	PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
	PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
	PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
  PREFIX dct: <http://purl.org/dc/terms/>

	INSERT { 
		GRAPH <${targetGraph}> {
    	<${newUri}> dct:hasPart ?newURI .
			?newURI ext:replacesPrevious ?agendaitem .
			<${newUri}> besluitvorming:heeftVorigeVersie ?agenda .
    	?newURI mu:uuid ?newUuid
		}
	} WHERE { { SELECT * WHERE {
    GRAPH <${targetGraph}> {
  		?agenda a besluitvorming:Agenda ;
			mu:uuid "${oldId}" .
			?agenda dct:hasPart ?agendaitem .

			OPTIONAL { ?agendaitem mu:uuid ?olduuid } 
			BIND(IF(BOUND(?olduuid), STRUUID(), STRUUID()) as ?uuid)
			BIND(IRI(CONCAT("http://localhost/vo/agendaitems/", ?uuid)) AS ?newURI)

		} } }
		BIND(STRAFTER(STR(?newURI), "http://localhost/vo/agendaitems/") AS ?newUuid) 
	}`

	await mu.update(createNewUris).catch(err => { console.error(err) });

	const moveProperties = `
	PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
	PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
	PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
	PREFIX dct: <http://purl.org/dc/terms/>
	PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>

	INSERT { 
		GRAPH <${targetGraph}> {
			?newURI ?p ?o .
			?s ?p2 ?newURI .
		}
	} WHERE {
		?newURI ext:replacesPrevious ?previousURI.
		FILTER NOT EXISTS {
			?newURI a besluit:Agendapunt .
		}
		OPTIONAL { 
		?previousURI ?p ?o .
		FILTER(?p != mu:uuid)
		}
		OPTIONAL { 
			?s ?p2 ?previousURI .
			FILTER(?p2 != dct:hasPart)
		}
	}`

	return await mu.update(moveProperties).catch(err => { console.error(err) });
}

const parseSparqlResults = (data) => {
	const vars = data.head.vars;
	return data.results.bindings.map(binding => {
		let obj = {};
		vars.forEach(varKey => {
			if (binding[varKey]) {
				obj[varKey] = binding[varKey].value;
			}
		});
		return obj;
	})
};

mu.app.use(mu.errorHandler);
