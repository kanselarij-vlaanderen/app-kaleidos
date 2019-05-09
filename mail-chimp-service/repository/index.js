import mu from 'mu';

const getNewsletterInfo = async (agendaId) => {

    const query = `
        PREFIX dct: <http://purl.org/dc/terms/>
        PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
        PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
        PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
        PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
        PREFIX dbpedia: <http://dbpedia.org/ontology/>
        PREFIX dct: <http://purl.org/dc/terms/>
        
        SELECT ?planned_start ?uuid ?title ?text ?subtitle ?publicationDate ?publicationDocDate ?finished WHERE {
            GRAPH <http://mu.semte.ch/application> {
               <http://data.lblod.info/id/agendas/${agendaId}> besluit:isAangemaaktVoor ?meeting .
               ?meeting besluit:geplandeStart ?planned_start .
               <http://data.lblod.info/id/agendas/${agendaId}> dct:hasPart ?agendaitem . 
               ?newsletter ext:nieuwsbriefInfo ?agendaitem . 
               OPTIONAL { ?newsletter mu:uuid ?uuid . }
               OPTIONAL { ?newsletter besluitvorming:inhoud ?text . }
               OPTIONAL { ?newsletter dbpedia:subtitle ?subtitle . }
               OPTIONAL { ?newsletter dct:issued ?publicationDate . }
               OPTIONAL { ?newsletter ext:issuedDocDate ?publicationDocDate . }
               OPTIONAL { ?newsletter dct:title ?title . }
               OPTIONAL { ?newsletter ext:afgewerkt ?finished . }
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



