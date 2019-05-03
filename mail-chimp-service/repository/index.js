import mu from 'mu';

const getNewsletterInfo = async (agendaId) => {

    const query = `
      PREFIX dct: <http://purl.org/dc/terms/>
        PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
        PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
        PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
        PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
        
        SELECT ?newsletter ?p ?o WHERE {
            GRAPH <http://mu.semte.ch/application> {
               ?agenda dct:hasPart ?agendapunt .
               ?agenda mu:uuid "${agendaId}" .
               ?agendapunt ext:nieuwsbriefInfo ?newsletter . 
               ?newsletter ?p ?o
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
    getNewsletterInfo
};



