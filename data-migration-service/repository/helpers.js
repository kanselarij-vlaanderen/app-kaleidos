import mu from 'mu';

const parseSparQlResults = (data, multiValueProperties = []) => {
	const vars = data.head.vars;
	return data.results.bindings.map(binding => {
		let obj = {};

		vars.forEach(varKey => {
			if (binding[varKey]){
				let val = binding[varKey].value;
				if (multiValueProperties.includes(varKey)){
					val = val.split('|')
				}
				obj[varKey] = val;
			}else {
				obj[varKey] = null;
			}
		});
		return obj;
	})
};

const removeInfoNotInTemp = (tempGraph, targetGraph) => {
  const query = `
  PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
  PREFIX dct: <http://purl.org/dc/terms/>
  PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
  PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
  PREFIX dbpedia: <http://dbpedia.org/ontology/>
  PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
  PREFIX nfo: <http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  DELETE {
    GRAPH <${targetGraph}> {
      ?s ?p ?o.
    }
  } where {
    GRAPH <${targetGraph}> {
			?s ?p ?o.
			?s a ?type.

      FILTER NOT EXISTS {
        GRAPH <${tempGraph}> {
          ?s ?p ?o.
        }
      }
    }
  }`;
  return mu.query(query);
};

const notConfidentialFilter = `
    FILTER NOT EXISTS {
      ?s ext:vertrouwelijk "true"^^<http://mu.semte.ch/vocabularies/typed-literals/boolean> .
    }
`;

const addRelatedFiles = (tempGraph, adminGraph) => {
  const query = `
  PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
  PREFIX dct: <http://purl.org/dc/terms/>
  PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
  PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
  PREFIX dbpedia: <http://dbpedia.org/ontology/>
  PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
  PREFIX nfo: <http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  INSERT {
    GRAPH <${tempGraph}> {
      ?s a nfo:FileDataObject .
      ?second a nfo:FileDataObject .
    }
  } where {
    GRAPH <${tempGraph}> {
      ?target a ?targetClass .
    }
    GRAPH <${adminGraph}> {
      ?s a nfo:FileDataObject .
      ?target ext:file ?s.

      OPTIONAL {
        ?second <http://www.semanticdesktop.org/ontologies/2007/01/19/nie#dataSource> ?s.
      }
    }
  }`;
  return mu.query(query);
};

const cleanup = (tempGraph) => {
  const query = `

  DROP SILENT GRAPH <${tempGraph}>`;
  return mu.query(query);
};

const fillOutDetailsOnVisibleItems = (tempGraph, targetGraph, adminGraph) => {
  const query = `
  PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
  PREFIX dct: <http://purl.org/dc/terms/>
  PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
  PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
  PREFIX dbpedia: <http://dbpedia.org/ontology/>
  PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
  PREFIX nfo: <http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  INSERT {
    GRAPH <${tempGraph}> {
      ?s a ?thing.
      ?s ?p ?o.
      ?oo ?pp ?s.
      ?s ?p ?literalo.
    }
    GRAPH <${targetGraph}> {
      ?s a ?thing.
      ?s ?p ?o.
      ?oo ?pp ?s.
      ?s ?p ?literalo.
    }
  } where {
    GRAPH <${adminGraph}> {
      ?s a ?thing.
      GRAPH <${tempGraph}> {
        ?s a ?thing .
      }
      ?s ?p ?literalo.
      FILTER(isLiteral(?literalo))
      OPTIONAL {
        ?oo ?pp ?s.
        GRAPH <${tempGraph}> {
          ?oo a ?oothing.
        }
      }
      OPTIONAL {
        ?s ?p ?o.
        GRAPH <${tempGraph}> {
          ?o a ?othing.
        }
      }
    }    
  }`;
  return mu.query(query);
};

const addAllRelatedDocuments = (tempGraph, adminGraph) => {
  const query = `
  PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
  PREFIX dct: <http://purl.org/dc/terms/>
  PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
  PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
  PREFIX dbpedia: <http://dbpedia.org/ontology/>
  PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
  PREFIX nfo: <http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  INSERT {
    GRAPH <${tempGraph}> {
      ?s a ?thing .
      ?version a ?subthing .
    }
  } where {
    GRAPH <${tempGraph}> {
      ?target a ?targetClass .
    }
    GRAPH <${adminGraph}> {
      ?s a ?thing .
      ?target ?p ?s .

      FILTER( ?thing IN(
        foaf:Document,
        ext:DocumentVersie ) )
      
      ${notConfidentialFilter}

      OPTIONAL {
        ?s besluitvorming:heeftVersie ?version.
        ?version a ?subthing.
      }
    }
  }`;
  return mu.query(query);
};

const addAllRelatedToAgenda = (tempGraph, adminGraph) => {
  const query = `
  PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
  PREFIX dct: <http://purl.org/dc/terms/>
  PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
  PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
  PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
  INSERT {
    GRAPH <${tempGraph}> {
      ?s a ?thing .
      ?subcase a dbpedia:UnitOfWork .
    }
  } where {
    GRAPH <${tempGraph}> {
      ?agenda a besluitvorming:Agenda .
    }
    GRAPH <${adminGraph}> {
      ?s a ?thing .
      FILTER NOT EXISTS {
        ?s a besluit:AgendaPunt .
        ?subcase besluitvorming:isGeagendeerdVia ?agendaItem .
      }
      { { ?s ?p ?agenda } UNION { ?agenda ?p ?s } }
      FILTER( ?thing NOT IN(besluitvorming:Agenda) )
      ${notConfidentialFilter}
    }
  }`;
  return mu.query(query);
};

const addRelatedToAgendaItemAndSubcase = (tempGraph, adminGraph) => {
  const query = `
  PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
  PREFIX dct: <http://purl.org/dc/terms/>
  PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
  PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
  PREFIX dbpedia: <http://dbpedia.org/ontology/>
  PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
  PREFIX nfo: <http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  INSERT {
    GRAPH <${tempGraph}> {
      ?s a ?thing .
    }
  } where {
    GRAPH <${tempGraph}> {
      ?target a ?targetClass .
      FILTER(?targetClass IN (besluit:Agendapunt, dbpedia:UnitOfWork))
    }
    GRAPH <${adminGraph}> {
      ?s a ?thing .
      { { ?s ?p ?target } UNION { ?target ?p ?s } }
      FILTER( ?thing NOT IN(
        besluitvorming:Agenda, 
        besluit:AgendaItem,
        dbpedia:UnitOfWork,
        foaf:Document,
        ext:DocumentVersie,
        nfo:FileDataObject ) )
      ${notConfidentialFilter}

    }
  }`;
  return mu.query(query);
};

module.exports = {
	parseSparQlResults,
	removeInfoNotInTemp,
	notConfidentialFilter,
	addRelatedFiles,
	cleanup,
	fillOutDetailsOnVisibleItems,
	addAllRelatedDocuments,
	addAllRelatedToAgenda,
	addRelatedToAgendaItemAndSubcase
};

