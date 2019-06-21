import mu from 'mu';
const targetGraph = "http://mu.semte.ch/graphs/organizations/kanselarij";

const getAllSessions = async () => {
	const firstDayOfTheYear = new Date(new Date().getFullYear(), 0, 1);
	const query = `
  PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
	PREFIX mu: <http://mu.semte.ch/vocabularies/core/>

  SELECT ?session WHERE {
    GRAPH <${targetGraph}> 
    {
      ?session a besluit:Zitting ;
      mu:uuid ?uuid ;
      besluit:geplandeStart ?plannedstart .
      FILTER(str(?plannedstart) > "${firstDayOfTheYear.toISOString()}")
    }
  }
  ORDER BY ASC(?plannedstart)
  LIMIT 366`
	const data = await mu.query(query);
	return parseSparqlResults(data);
};

const getClosestMeeting = async (date) => {
	const query = `
  PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
	PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
	PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
  
  SELECT ?session ?meeting_id ?plannedstart ?agendaName ?agenda_id ?creationDate WHERE {
    GRAPH <${targetGraph}> 
    {
      ?session a besluit:Zitting ;
      mu:uuid ?meeting_id ;
			besluit:geplandeStart ?plannedstart .
			?agendas besluit:isAangemaaktVoor ?session ;
			mu:uuid ?agenda_id ;
			ext:agendaNaam ?agendaName ;
			ext:aangemaaktOp ?creationDate .
			FILTER(str(?plannedstart) < "${date.toISOString()}")
    }
  }
  ORDER BY DESC(?plannedstart) DESC(?creationDate)
	LIMIT 1`

	let data = await mu.query(query);
	return parseSparqlResults(data);
};

const updateSessionNumbers = async (sessions) => {
	let toDelete = [];
	let insertString = "";

	sessions.forEach(obj => {
		toDelete.push(`<${obj.session}>`);
		insertString = `${insertString}
    <${obj.session}> adms:identifier """${obj.number}"""^^xsd:decimal .
    `
	})

	const deleteString = toDelete.join();

	const query = `
  PREFIX adms: <http://www.w3.org/ns/adms#>
  
  DELETE WHERE { 
    GRAPH <${targetGraph}> { 
      ?target adms:identifier ?o .
      FILTER(?target IN (${deleteString}))
    } 
  };

  INSERT DATA { 
    GRAPH <${targetGraph}> { 
      ${insertString}
    } 
  }
  `
	return mu.update(query);
}

const parseSparqlResults = (data) => {
	const vars = data.head.vars;
	return data.results.bindings.map(binding => {
		let obj = {};

		vars.forEach(varKey => {
			if (binding[varKey]) {
				obj[varKey] = binding[varKey].value;
			} else {
				obj[varKey] = null;
			}
		});
		return obj;
	})
};

module.exports = {
	getAllSessions,
	getClosestMeeting,
	updateSessionNumbers
};
