import mu from 'mu';

const getPostponedSubcases = async () => {

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
      
      SELECT ?uuid ?title ?created
        WHERE { 
          GRAPH <http://mu.semte.ch/application>
          {
            ?subcase besluitvorming:isGeagendeerdVia ?agendapunt ;
             mu:uuid ?uuid ;
             dct:title ?title ;
             dct:created ?created .
            ?agendapunt ext:heeftVerdaagd ?verdaagd .
           }
      } GROUP BY ?subcase`;

    let data = await mu.query(query);
    return parseSparqlResults(data);
}


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
module.exports = {
    getPostponedSubcases
};
