import mu from 'mu';

const getRelatedSubCasesOfAgenda = async (agendaId) => {

    const query = `
      PREFIX dct: <http://purl.org/dc/terms/>
        PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
        PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
        PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
        PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
        
    SELECT ?meeting ?agendapunt ?subcase ?decision ?retracted ?postponed ?archived ?concluded WHERE {
            GRAPH <http://mu.semte.ch/application> {
               ?agenda mu:uuid "${agendaId}" .
           ?agenda dct:hasPart ?agendaitem .
           ?subcase besluitvorming:isGeagendeerdVia ?agendapunt ;
                    ext:isProcedurestapGearchiveerd ?archived ;
                    besluitvorming:besloten ?concluded .
               OPTIONAL { ?agenda besluit:isAangemaaktVoor ?meeting . }
               OPTIONAL { ?agendapunt besluitvorming:ingetrokken ?retracted . }
           OPTIONAL { ?subcase ext:procedurestapHeeftBesluit ?decision . }
               OPTIONAL { ?agendapunt ext:heeftVerdaagd ?postponed . }
          
             } 
        }`;

    let data = await mu.query(query);
    return parseSparqlResults(data);
};

const concludeSubCases = async (subcases) => {

    const oldPriorities = subcases.map((subcase, index) =>
      ` <${subcase.uri}> besluitvorming:besloten ?o${index} . 
        `).join(' ');
    const newPriorities = subcases.map(subcase =>
      ` <${subcase.uri}> besluitvorming:besloten "true"^^xsd:boolean .
        `).join(' ');

    const query = `
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
      PREFIX xsd:  <http://mu.semte.ch/vocabularies/typed-literals/>
      
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
    console.log('conclude')
    return mu.update(query);
}

const retractAgendaItems = async (items) => {

    const oldPriorities = items.map((agendaItem, index) =>
      ` <${agendaItem.uri}> besluitvorming:ingetrokken ?o${index} . 
        `).join(' ');
    const newPriorities = items.map(agendaItem =>
      ` <${agendaItem.uri}> besluitvorming:ingetrokken "true"^^xsd:boolean .
        `).join(' ');

    const query = `
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
      PREFIX xsd:  <http://mu.semte.ch/vocabularies/typed-literals/>
      
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
}

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

const finaliseMeeting = (meeting) => {
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
      console.log('finalise')

    return mu.update(query);
};

module.exports = {
    getRelatedSubCasesOfAgenda, concludeSubCases, retractAgendaItems, finaliseMeeting
};



