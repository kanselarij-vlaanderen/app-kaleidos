import mu from 'mu';

const getMinisters = async () => {

    const query = `
        PREFIX core: <http://mu.semte.ch/vocabularies/core/>
            PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
            PREFIX persoon: <http://data.vlaanderen.be/ns/persoon#>
            PREFIX dct: <http://purl.org/dc/terms/>
            PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
            PREFIX foaf: <http://xmlns.com/foaf/0.1/>
            PREFIX adms: <http://www.w3.org/ns/adms#>
            PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

            SELECT *
            WHERE {
                GRAPH <http://mu.semte.ch/application> {
                    ?mandatee a mandaat:Mandataris ;
                    dct:title ?title ;
                    mandaat:isBestuurlijkeAliasVan ?person .
                    ?person foaf:familyName ?familyName ;
                    foaf:firstName ?firstName
                }
            }`;
    let data = await mu.query(query);
    return parseSparqlResults(data);
};

const getMinistersForDomain = async () => {

    const query = `
        PREFIX core: <http://mu.semte.ch/vocabularies/core/>
        PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
        PREFIX persoon: <http://data.vlaanderen.be/ns/persoon#>
        PREFIX dct: <http://purl.org/dc/terms/>
        PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        PREFIX adms: <http://www.w3.org/ns/adms#>
        PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
        PREFIX org: <http://www.w3.org/ns/org#>

        SELECT *
        WHERE {
            GRAPH <http://mu.semte.ch/application> {
                ?domain a ext:BeleidsdomeinCode ;
                skos:prefLabel ?label ;
                skos:scopeNote ?note .
                ?mandatee mandaat:beleidsdomein ?domain .
                ?mandatee mandaat:isBestuurlijkeAliasVan ?person .
                ?person foaf:familyName ?familyName ;
                foaf:firstName ?firstName
            }
        }`;
    let data = await mu.query(query);
    return parseSparqlResults(data);
};

const getMandateeForDomain = async (domain) => {
    const query = `
        PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        SELECT *
        WHERE {{
            GRAPH <http://mu.semte.ch/application> {{
                ?mandatee mandaat:beleidsdomein <${domain}> .
                ?mandatee mandaat:isBestuurlijkeAliasVan ?person .
                ?person foaf:familyName ?familyName ;
                foaf:firstName ?firstName
            }}
        }}`;
    let data = await mu.query(query);
    return parseSparqlResults(data);
};

const getDomainsForMandatee = async (mandatee) => {
    const query = `
        PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
        PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    
        SELECT *
        WHERE {{
            GRAPH <http://mu.semte.ch/application> {{
                <${mandatee}> mandaat:beleidsdomein ?domain .
                ?domain skos:prefLabel ?label
            }}
        }}`;
    let data = await mu.query(query);
    return parseSparqlResults(data);
};

const getUniqueSubCaseWhereOpenByMandatee = async (mandatee) => {
    const query = `
        PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
        PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
        PREFIX dct: <http://purl.org/dc/terms/>
        PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
        PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
        PREFIX mu: <http://mu.semte.ch/vocabularies/core/>

        SELECT * WHERE {
           GRAPH <http://mu.semte.ch/graphs/organizations/kanselarij> {
              ?uri besluitvorming:heeftBevoegde <http://kanselarij.vo.data.gift/id/mandatarissen/${mandatee}> . 
              ?uri besluitvorming:besloten "false"^^<http://mu.semte.ch/vocabularies/typed-literals/boolean> .
           }
        }`;
    let data = await mu.query(query);
    return parseSparqlResults(data);
};

const setMandateeOnDomain = async (receiving_mandatee, domain) => {
    const query = `
        PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
        PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

        DELETE WHERE {{
            GRAPH <http://mu.semte.ch/application> {{
                ?mandatee mandaat:beleidsdomein <${domain}> .
            }}
        }}

        INSERT DATA {{
            GRAPH <http://mu.semte.ch/application> {{
                <${receiving_mandatee}> mandaat:beleidsdomein <${domain}> .
            }}
        }}`;
    return await mu.update(query).catch(err => { console.error(err) });
};

const addMandateeToSubCase = async (receiving_mandatee, domain) => {
    const query = `
        PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
        PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

        DELETE WHERE {{
            GRAPH <http://mu.semte.ch/application> {{
                ?mandatee mandaat:beleidsdomein <${domain}> .
            }}
        }}

        INSERT DATA {{
            GRAPH <http://mu.semte.ch/application> {{
                <${receiving_mandatee}> mandaat:beleidsdomein <${domain}> .
            }}
        }}`;
    return await mu.update(query).catch(err => { console.error(err) });
};

const setMandateeOnSubCase = async (open_sub_cases, new_mandatee, old_mandatee) => {
    for (let i = 0; i < open_sub_cases.length; i ++){
        const subcase = open_sub_cases[i];
        console.log(subcase);
        mu.update(`
        PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>

        DELETE WHERE {
            GRAPH <http://mu.semte.ch/graphs/organizations/kanselarij> {
                <${subcase.uri}> besluitvorming:heeftBevoegde <http://kanselarij.vo.data.gift/id/mandatarissen/${old_mandatee}> .
            }
        }

        INSERT {
             GRAPH <http://mu.semte.ch/graphs/organizations/kanselarij> {
                   <${subcase.uri}> besluitvorming:heeftBevoegde <http://kanselarij.vo.data.gift/id/mandatarissen/${new_mandatee}> .
            }
        }`).catch(err => { console.error(err) });
    }
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
    getMinisters,
    getMinistersForDomain,
    getMandateeForDomain,
    getDomainsForMandatee,
    setMandateeOnDomain,
    addMandateeToSubCase,
    getUniqueSubCaseWhereOpenByMandatee,
    setMandateeOnSubCase
};




