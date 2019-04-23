import mu from 'mu';

const getRelatedSubCasesOfAgenda = async (agendaId) => {

    const query = `
      PREFIX dct: <http://purl.org/dc/terms/>
        PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
        PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
        PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
        PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
        
        SELECT ?meeting ?agendapunt ?uri ?decision ?retracted ?postponed ?archived ?concluded WHERE {
            GRAPH <http://mu.semte.ch/application> {
               ?agenda dct:hasPart ?agendapunt .
               ?agenda mu:uuid "${agendaId}" .
               OPTIONAL { ?agenda besluit:isAangemaaktVoor ?meeting . }
               OPTIONAL { ?agendapunt besluitvorming:ingetrokken ?retracted . }
               OPTIONAL { ?agendapunt ext:agendapuntHeeftBesluit ?decision . }
               OPTIONAL { ?agendapunt ext:heeftVerdaagd ?postponed . }
               ?uri besluitvorming:isGeagendeerdVia ?agendapunt ;
                        ext:isProcedurestapGearchiveerd ?archived ;
                        besluitvorming:besloten ?concluded
             }
        }`;
    let data = await mu.query(query);
    return parseSparqlResults(data);
};

const parseSparqlResults = (data) => {
    const vars = data.head.vars;
    return data.results.bindings.map(binding => {
        let obj = {};

        vars.forEach(varKey => {
            if (binding[varKey]){
                obj[varKey] = binding[varKey].value;
            }else {
                obj[varKey] = null;
            }
        });
        return obj;
    })
};

const migrateGraph = (meeting) => {
    const query = `
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
      PREFIX xsd:  <http://mu.semte.ch/vocabularies/typed-literals/>
      
      DELETE WHERE { 
          GRAPH ?g {
            <${meeting}> besluitvorming:finaleZittingVersie ?o . 
          }        
      };
    
      INSERT DATA { 
        GRAPH <http://mu.semte.ch/application> { 
          <${meeting}> besluitvorming:finaleZittingVersie "true"^^xsd:boolean .
        } 
      }`;
    return mu.update(query);
};

module.exports = {
    migrateGraph
};



