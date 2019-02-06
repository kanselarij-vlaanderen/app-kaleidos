import mu from 'mu';

const getMinistersWithBevoegdheidByAgendaId = async (agendaId) => {

    const query = `
      PREFIX vo-org: <https://data.vlaanderen.be/ns/organisatie#>
      PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
      PREFIX vo-gen: <https://data.vlaanderen.be/ns/generiek#> 
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      PREFIX vo-besluit: <https://data.vlaanderen.be/ns/besluitvorming#>
      PREFIX agenda: <http://localhost/vo/agendas/>
      
      SELECT ?uuid ?agendapunt ?priority ?ministerId ?minister ?responsibility 
        WHERE { 
          GRAPH <http://mu.semte.ch/application>
          {
            agenda:${agendaId} ext:agendapunt ?agendapunt .
            ?agendapunt mu:uuid ?uuid .
            OPTIONAL { ?agendapunt ext:prioriteit ?priority . }
            ?agendapunt vo-besluit:subcase ?subcase . 
            ?case ext:deeldossier ?subcase ;
                  vo-besluit:bevoegde ?hoedanigheid .
            ?hoedanigheid skos:prefLabel ?minister ; 
                          mu:uuid ?ministerId ;
                          vo-org:bevoegdheid ?bevoegdheid .
            ?bevoegdheid skos:prefLabel ?responsibility
           }
      }`;

    let data = await mu.query(query);
    const results = parseSparqlResults(data);
    return parseMinistersWithBevoegdheden(results);
}


const updateAgendaItemPriority = async (items) => {

    const oldPriorities = items.map(item =>
        ` <${item.agendapunt}> ext:prioriteit ${item.priority} . 
        `).join(' ');
    const newPriorities = items.map(item =>
        ` <${item.agendapunt}> ext:prioriteit ${item.newPriority} .
        `).join(' ');

    const query = `
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      
      DELETE DATA { 
        GRAPH <http://mu.semte.ch/application> { 
          ${oldPriorities}
        } 
      }
    
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

// TODO Refactor naar functies in de plaats van grote blocks

const parseMinistersWithBevoegdheden = (items) => {
    let agendaItems = {};

    for (let i = 0; i < items.length; i++){

        const agendaItem = items[i];
        const uuid = agendaItem.uuid;

        if (agendaItems[uuid]){

            agendaItems[uuid].connections.push({
                ministerId: agendaItem.ministerId,
                minister: agendaItem.minister,
                responsibility: agendaItem.responsibility
            });

        }else {

            if (agendaItem.priority){
                agendaItem.priority = parseInt(agendaItem.priority);
            }else {
                agendaItem.priority = 999;
            }
            agendaItem.priority = parseInt(agendaItem.priority);
            agendaItem.connections = [{
                ministerId: agendaItem.ministerId,
                minister: agendaItem.minister,
                responsibility: agendaItem.responsibility
            }];
            delete agendaItem.ministerId;
            delete agendaItem.minister;
            delete agendaItem.bevoegdOver;
            agendaItems[uuid] = agendaItem;

        }

    }
    return agendaItems;
};

module.exports = {
    getMinistersWithBevoegdheidByAgendaId, updateAgendaItemPriority
};
