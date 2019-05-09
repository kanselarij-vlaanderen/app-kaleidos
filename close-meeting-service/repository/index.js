import mu from 'mu';
const targetGraph = "http://mu.semte.ch/graphs/organizations/kanselarij";

const getRelatedSubCasesOfAgenda = async (agendaId) => {

    const query = `
      PREFIX dct: <http://purl.org/dc/terms/>
PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
        
    SELECT ?agendaitem ?subcase ?decision ?retracted ?postponed ?archived ?concluded WHERE {
       GRAPH <${targetGraph}> {
           <http://data.lblod.info/id/agendas/${agendaId}> dct:hasPart ?agendaitem . 
            ?subcase besluitvorming:isGeagendeerdVia ?agendaitem .
            OPTIONAL { ?subcase ext:isProcedurestapGearchiveerd ?archived . }
            OPTIONAL { ?subcase besluitvorming:besloten ?concluded . }
            OPTIONAL { ?agendaitem besluitvorming:isGeagendeerdVia ?subcase . }
            OPTIONAL { ?subcase ext:isProcedurestapGearchiveerd ?archived . }
            OPTIONAL { ?subcase besluitvorming:besloten ?concluded . }
            OPTIONAL { ?agendaitem besluitvorming:ingetrokken ?retracted . }
            OPTIONAL { ?subcase ext:procedurestapHeeftBesluit ?decision . }
            OPTIONAL { ?agendaitem ext:heeftVerdaagd ?postponed . }
 
       } 
    }
`;

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
        GRAPH <${targetGraph}> { 
          ${newPriorities}
        } 
      }`;

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
        GRAPH <${targetGraph}> { 
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
        GRAPH <${targetGraph}> { 
          <${meeting}> besluitvorming:finaleZittingVersie "true"^^xsd:boolean .
        } 
      }`;
      console.log('finalise')

    return mu.update(query);
};

module.exports = {
    getRelatedSubCasesOfAgenda, concludeSubCases, retractAgendaItems, finaliseMeeting
};



