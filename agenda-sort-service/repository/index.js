import mu from 'mu';
const targetGraph = "http://mu.semte.ch/graphs/organizations/kanselarij";

const getLastPriorityOfAgendaitemInAgenda = async (agendaId) => {
    const query = `
    PREFIX vo-org: <https://data.vlaanderen.be/ns/organisatie#>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    PREFIX vo-gen: <https://data.vlaanderen.be/ns/generiek#> 
    PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
    PREFIX agenda: <http://data.lblod.info/id/agendas/>
    PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
    PREFIX dct: <http://purl.org/dc/terms/>

 	SELECT MAX(?agendaitemPrio) AS ?maxPrio  WHERE {
  	GRAPH <${targetGraph}> {
   		?agenda a besluitvorming:Agenda ;
   		mu:uuid "${agendaId}" .
        ?agenda dct:hasPart ?agendaitem .
        OPTIONAL {?agendaitem ext:prioriteit ?agendaitemPrio .}
  	  }
     }`;

    let data = await mu.query(query);
    return parseSparqlResults(data);
}

const getAgendaPriorities = async (agendaId) => {
    const query = `
      PREFIX vo-org: <https://data.vlaanderen.be/ns/organisatie#>
      PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
      PREFIX vo-gen: <https://data.vlaanderen.be/ns/generiek#> 
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
      PREFIX agenda: <http://data.lblod.info/id/agendas/>
      PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
      PREFIX dct: <http://purl.org/dc/terms/>
      
      SELECT ?uuid ?agendapunt MIN(?priority) AS ?priority COUNT(DISTINCT(?mandatee)) AS ?mandateeCount
        WHERE { 
          GRAPH <${targetGraph}>
          {
            ?agenda dct:hasPart ?agendapunt .
            ?agenda mu:uuid "${agendaId}" .
            ?agendapunt mu:uuid ?uuid .
                FILTER NOT EXISTS{
                    ?agendapunt ext:prioriteit ?agendaitemPrio .
                }
                ?subcase besluitvorming:isGeagendeerdVia ?agendapunt .
            OPTIONAL { 
                ?subcase besluitvorming:heeftBevoegde ?mandatee . 
                ?mandatee mu:uuid ?mandateeId .
                ?mandatee mandaat:rangorde ?priority .
                ?mandatee mandaat:start ?start .
                FILTER(?start < NOW())
                OPTIONAL {
                   ?mandatee mandaat:eind ?end .
                   FILTER(?end > NOW())
                }
            }
           }
      } GROUP BY ?uuid ?agendapunt`;

    let data = await mu.query(query);
    const results = parseSparqlResults(data);
    return parsePriorityResults(results);
}

const updateAgendaItemPriority = async (items) => {

    const oldPriorities = items.map(item => {
        return ` <${item.uri}> ext:prioriteit ${item.priority} .`;
    });

    const newPriorities = items.map(item => {
        if (!item.agendaitemPrio) {
            return ` <${item.uri}> ext:prioriteit ${item.priority} .`;
        }
    });

    console.log(oldPriorities);
    console.log(newPriorities);
    // return;
    if (newPriorities.length < 1) {
        return;
    }

    const query = `
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      
      DELETE WHERE { 
          GRAPH ?g {
            ${oldPriorities.join(' ')}
          }        
      };
    
      INSERT DATA { 
        GRAPH <${targetGraph}> { 
          ${newPriorities.join(' ')}
        } 
      }`;

    return mu.update(query);
};

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

const parsePriorityResults = (items) => {
    let agendaItems = {};

    items.map((agendaItem) => {
        const uuid = agendaItem.uuid;
        agendaItem.priority = agendaItem.priority || Number.MAX_SAFE_INTEGER;

        if (agendaItems[uuid]) {
            agendaItems[uuid].mandatePriority = Math.min(agendaItems[uuid].mandatePriority, agendaItem.priority);
        } else {
            agendaItems[uuid] = {
                uuid: uuid,
                uri: agendaItem.agendapunt,
                agendaitemPrio: agendaItem.agendaitemPrio,
                mandatePriority: agendaItem.priority,
                mandateeCount: agendaItem.mandateeCount
            }
        }
    });
    return Object.values(agendaItems);
};

module.exports = {
    getAgendaPriorities, updateAgendaItemPriority, getLastPriorityOfAgendaitemInAgenda
};
