import mu from 'mu';

const getAgendaWhereisMostRecentAndFinal = async (agendaId) => {

    const query = `
        PREFIX dct: <http://purl.org/dc/terms/>
        PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
        PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
        PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
        PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
        PREFIX dbpedia: <http://dbpedia.org/ontology/>
        PREFIX dct: <http://purl.org/dc/terms/>
        PREFIX prov: <http://www.w3.org/ns/prov#>
        
        SELECT DISTINCT ?agenda ?p ?o WHERE {
              GRAPH <http://mu.semte.ch/graphs/organizations/kanselarij> {
            ?s mu:uuid ?uuid; a besluit:Zitting .
          OPTIONAL { ?agenda besluit:isAangemaaktVoor ?s }
        OPTIONAL { ?agenda ?p ?o }
        }
        }`;
    let data = await mu.query(query);
    return parseSparqlResults(data);
};

const getNewsLetterByAgendaId = async (agendaId) => {
    const query = `
        PREFIX dct: <http://purl.org/dc/terms/>
        PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
        PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
        PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
        PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
        PREFIX dbpedia: <http://dbpedia.org/ontology/>
        PREFIX dct: <http://purl.org/dc/terms/>
        PREFIX prov: <http://www.w3.org/ns/prov#>
        
        SELECT ?planned_start ?uuid ?title ?richtext ?text ?subtitle WHERE {
            GRAPH <http://mu.semte.ch/application> {
               <http://data.lblod.info/id/agendas/${agendaId}> besluit:isAangemaaktVoor ?meeting .
               ?meeting besluit:geplandeStart ?planned_start .
               <http://data.lblod.info/id/agendas/${agendaId}> dct:hasPart ?agendaitem . 
               ?agendaitem prov:generated  ?newsletter . 
               OPTIONAL { ?newsletter mu:uuid ?uuid . }
               OPTIONAL { ?newsletter besluitvorming:inhoud ?text . }
               OPTIONAL { ?newsletter ext:htmlInhoud ?richtext . }
               OPTIONAL { ?newsletter dbpedia:subtitle ?subtitle . }
               OPTIONAL { ?newsletter dct:title ?title . }
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


module.exports = {
    getNewsLetterByAgendaId,
    getAgendaWhereisMostRecentAndFinal
};




