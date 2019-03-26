// VIRTUOSO bug: https://github.com/openlink/virtuoso-opensource/issues/515
import mu from 'mu';
import { ok } from 'assert';
import cors from 'cors';
const uuidv4 = require('uuid/v4');

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
	await ensureDocumentsHasSerialnumberForSession(oldAgendaId);
	await nameSerialNumbersForSession(oldAgendaId);

	const codeURI = await getSubcasePhaseCode();
	const subcasePhasesOfAgenda = await getSubcasePhasesOfAgenda(newAgendaId, codeURI);

	await checkForPhasesAndAssignMissingPhases(subcasePhasesOfAgenda, codeURI);
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
		GRAPH <http://mu.semte.ch/application> {
					?code a ext:ProcedurestapFaseCode ;
                  skos:prefLabel ?label .
           				FILTER(UCASE(?label) = UCASE("geagendeerd"))  
		}
	}
`
	const data = await mu.query(query).catch(err => { console.error(err) });
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
		GRAPH <http://mu.semte.ch/application> {
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
	 GRAPH <http://mu.semte.ch/application> {
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
  	GRAPH <http://mu.semte.ch/application> {
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
	const query = `
	PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
	PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
	PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
  PREFIX dct: <http://purl.org/dc/terms/>

	INSERT { 
		GRAPH <http://mu.semte.ch/application> {
    	<${newUri}> dct:hasPart ?newURI .
    	?newURI ?p ?o .
    	?s ?p2 ?newURI .
    	?newURI mu:uuid ?newUuid
		}
	} WHERE { { SELECT * WHERE {
    GRAPH <http://mu.semte.ch/application> {
  		?agenda a besluitvorming:Agenda ;
			mu:uuid "${oldId}" .
			?agenda dct:hasPart ?agendaitem .

			OPTIONAL { ?agendaitem mu:uuid ?olduuid } 
			BIND(IF(BOUND(?olduuid), STRUUID(), STRUUID()) as ?uuid)
			BIND(IRI(CONCAT("http://localhost/vo/agendaitems/", ?uuid)) AS ?newURI)

			OPTIONAL { 
				SELECT ?agendaitem ?p ?o
				WHERE {
					?agendaitem ?p ?o .
					FILTER(?p != mu:uuid)
				}
			}
			OPTIONAL { 
				SELECT ?agendaitem ?s ?p2 
				WHERE {
					?s ?p2 ?agendaitem .
					FILTER(?p2 != dct:hasPart)
				}
			}    
		} } }
		BIND(STRAFTER(STR(?newURI), "http://localhost/vo/agendaitems/") AS ?newUuid) 
	}
`

	return await mu.update(query).catch(err => { console.error(err) });
}

async function ensureDocumentsHasSerialnumberForSession(agendaId) {
	const query = `
	PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
	PREFIX ext: <http://mu.semte.ch/vocabularies/ext/> 
	PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
	PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
		
	INSERT { 
		GRAPH <http://mu.semte.ch/application> {
			?identifier a ext:DocumentIdentifier .
     	?identifier ext:identifiesVersion ?versie .
			?identifier mu:uuid ?newUUID .
			?identifier ext:versieNummer 1 .
			?identifier ext:meeting ?session .
			?identifier ext:procedurestap ?procedurestap .
	  }
	} where {
		{ SELECT * WHERE {
			GRAPH <http://mu.semte.ch/application> {
				?agenda a besluitvorming:Agenda .
				?agenda mu:uuid "${agendaId}" .
			  ?agenda besluit:isAangemaaktVoor ?session .
			  ?agenda <http://purl.org/dc/terms/hasPart> ?agendaitem .
			  ?procedurestap besluitvorming:isGeagendeerdVia ?agendaitem .
			  ?procedurestap ext:bevatDocumentversie ?versie .
				FILTER NOT EXISTS { 
				  ?identifier ext:identifiesVersion ?versie .
					?identifier ext:meeting ?session .
				}
				OPTIONAL {
					?versie mu:uuid ?versionid .
				}
				BIND(IF(BOUND(?versionid), STRUUID(), STRUUID()) AS ?newUUID)
		} } }
		BIND(IRI(CONCAT("http://mu.semte.ch/vocabularies/ext/identifiers/",?newUUID)) AS ?identifier)
	}`
	return await mu.update(query).catch(err => { console.error(err) });
}

async function nameSerialNumbersForSession(agendaId) {
	const query = `PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
	PREFIX ext: <http://mu.semte.ch/vocabularies/ext/> 
	PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
	PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
	PREFIX dct: <http://purl.org/dc/terms/>
		
	SELECT ?procedurestap ?identifiers ?namedIdentifierCount WHERE {
					?agenda a besluitvorming:Agenda.
					?agenda mu:uuid "${agendaId}".
					?agenda dct:hasPart ?agendaitem.
					?procedurestap besluitvorming:isGeagendeerdVia ?agendaitem.
		{ SELECT ?procedurestap GROUP_CONCAT(DISTINCT ?identifier;separator="|") as ?identifiers WHERE { 
			GRAPH <http://mu.semte.ch/application> {
				?identifier a ext:DocumentIdentifier .
				?identifier ext:identifiesVersion ?versie.
				?identifier ext:procedurestap ?procedurestap
				FILTER NOT EXISTS {
						?identifier ext:serialNumber ?number.
				}
			}
		} GROUP BY ?procedurestap }
		{ SELECT ?procedurestap COUNT(DISTINCT(?identifier2)) AS ?namedIdentifierCount WHERE {
				GRAPH <http://mu.semte.ch/application> {
					?agenda a besluitvorming:Agenda.
					?agenda mu:uuid "${agendaId}".
					?agenda dct:hasPart ?agendaitem.
					?procedurestap besluitvorming:isGeagendeerdVia ?agendaitem.
					OPTIONAL {
						?identifier2 a ext:DocumentIdentifier .
						?identifier2 ext:procedurestap ?procedurestap.
						?identifier2 ext:serialNumber ?number. 
					}  
				}                	
		} GROUP BY ?procedurestap }
	}`
	let agendaItemInfo = await mu.query(query).catch(err => { console.error(err); });
	agendaItemInfo = parseSparqlResults(agendaItemInfo);
	let documentTypeMapping = await getDocumentTypesForDocsInAgenda(agendaId);
	let uriToSerialnumbermapping = {};
	agendaItemInfo.map((item) => {
		let { identifiers, namedIdentifierCount } = item;
		namedIdentifierCount = parseInt(namedIdentifierCount) + 1;
		identifiers = identifiers.split("|");
		sortIdentifiersByType(identifiers, documentTypeMapping);
		identifiers.map((identifier, index) => {
			let serialNumber = `${namedIdentifierCount + index}`;
			uriToSerialnumbermapping[identifier] = serialNumber;
		});
	});
	await updateSerialNumbersOfDocumentVersions(uriToSerialnumbermapping);
}

function sortIdentifiersByType(identifiers, typeMapping) {
	let notaURI = "http://http://data.vlaanderen.be/ns/besluitvorming/voc/besluit-type/9e5b1230-f3ad-438f-9c68-9d7b1b2d875d";
	let besluitURI = "http://http://data.vlaanderen.be/ns/besluitvorming/voc/besluit-type/4c7cfaf9-1d5f-4fdf-b7e9-b7ce5167e31a";
	let ontwerpdecreetURI = "http://http://data.vlaanderen.be/ns/besluitvorming/voc/besluit-type/f57a69b8-e4c1-468a-97ee-a516bb62c6b6";
	let decreetURI = "http://http://data.vlaanderen.be/ns/besluitvorming/voc/besluit-type/e4f73ddc-1ed6-4878-b9ed-ace55c0a8d64";

	let scoreMapping = {};
	scoreMapping[notaURI] = 1;
	scoreMapping[besluitURI] = 2;
	scoreMapping[ontwerpdecreetURI] = 2;
	scoreMapping[decreetURI] = 2;

	identifiers.sort((one, two) => {
		let oneType = typeMapping[one];
		let twoType = typeMapping[two];
		let oneScore = oneType ? scoreMapping[oneType] : 3;
		oneScore = oneScore || 3;
		let twoScore = twoType ? twoType[twoType] : 3;
		twoScore = twoScore || 3;
		return one < two ? -1 : 1;
	});
	return identifiers;
}

async function getDocumentTypesForDocsInAgenda(agendaId) {
	const query = `PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
	PREFIX ext: <http://mu.semte.ch/vocabularies/ext/> 
	PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
	PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
	PREFIX dct: <http://purl.org/dc/terms/>
	
	SELECT ?versie ?type WHERE {
		GRAPH <http://mu.semte.ch/application> {
			  ?agenda mu:uuid "${agendaId}".
			  ?agenda dct:hasPart ?agendaitem.
			  ?identifier ext:agendaitem ?agendaitem.
				?identifier a ext:DocumentIdentifier .
				?identifier ext:identifiesVersion ?versie.
				?identifier ext:meeting ?meeting.
				FILTER NOT EXISTS {
						?identifier ext:serialNumber ?number.
				}
				?document besluitvorming:heeftVersie ?versie.
				?document ext:documentType ?type
		}
	}`;
	let versieTypes = await mu.query(query).catch(err => { console.error(err); });
	versieTypes = parseSparqlResults(versieTypes);
	let mapping = {};
	versieTypes.map((tuple) => {
		mapping[tuple.versie] = tuple.type;
	});
	return mapping;
}

async function updateSerialNumbersOfDocumentVersions(serialnumberMap) {
	let insertString = [];

	Object.keys(serialnumberMap).map((identifier) => {
		insertString.push(`<${identifier}> ext:serieNummer "${serialnumberMap[identifier]}" .`);
	})

	if (insertString.length <= 0) {
		return;
	}
	insertString = insertString.join("\n");

	const queryString = `
		PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

		INSERT DATA { 
			GRAPH <http://mu.semte.ch/application> { 
				${insertString}
			}
		}
	`

	return await mu.update(queryString).catch(err => { console.error(err) });
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
