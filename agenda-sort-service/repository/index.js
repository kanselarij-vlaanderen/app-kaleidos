import mu from 'mu';

const getMinistersWithBevoegdheidByAgendaId = async (agendaId) => {

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
      
      SELECT ?uuid ?agendapunt ?mandateeId ?priority
        WHERE { 
          GRAPH <http://mu.semte.ch/application>
          {
            ?agenda dct:hasPart ?agendapunt .
            ?agenda mu:uuid "${agendaId}" .
            ?agendapunt mu:uuid ?uuid .
            ?subcase besluitvorming:isGeagendeerdVia ?agendapunt .
            OPTIONAL { ?subcase besluitvorming:heeftBevoegde ?mandatee . }
            OPTIONAL { ?mandatee mu:uuid ?mandateeId . }
            OPTIONAL { ?mandatee mandaat:rangorde ?priority . }
           }
      }`;

    let data = await mu.query(query);
    const results = parseSparqlResults(data);
    return parseMandateeWithTheirMandatePriority(results);
}

const updateAgendaItemPriority = async (items) => {

    const oldPriorities = items.map(item =>
        ` <${item.agendapunt}> ext:prioriteit ?priority . 
        `).join(' ');
    const newPriorities = items.map(item =>
        ` <${item.agendapunt}> ext:prioriteit ${item.newPriority} .
        `).join(' ');

    const query = `
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      
      DELETE WHERE { 
          GRAPH ?g {
            ${oldPriorities}
          }        
      };
    
      INSERT DATA { 
        GRAPH <http://mu.semte.ch/application> { 
          ${newPriorities}
        } 
      }`;
     return mu.update(query);
};

const parseSparqlResults = (data) => {
    const vars = data.head.vars;
    return data.results.bindings.map(binding => {
        let obj = {};
        vars.forEach(varKey => {
            if (binding[varKey]){
                obj[varKey] = binding[varKey].value;
            }
        });
        return obj;
    })
};

const parseMandateeWithTheirMandatePriority = (items) => {
    let agendaItems = {};

    for (let i = 0; i < items.length; i++){

        const agendaItem = items[i];
        const uuid = agendaItem.uuid;

        if (agendaItems[uuid]){

            agendaItems[uuid].mandates.push({
                priority: agendaItem.priority,
                mandateeId: agendaItem.mandateeId
            });

        }else {

            agendaItem.mandates = [{
              priority: agendaItem.priority,
              mandateeId: agendaItem.mandateeId
            }];
            agendaItems[uuid] = agendaItem;
        }

    }
    return agendaItems;
};

module.exports = {
    getMinistersWithBevoegdheidByAgendaId, updateAgendaItemPriority
};
