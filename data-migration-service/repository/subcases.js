import mu from 'mu';
import { parseSparQlResults } from './helpers';

const has_many = ['approvals', 'themes', 'creators', 'mandatees', 'government_domains', 'relates_to', 'document_versions', 'document_identifiers',
    'consultation_requests', 'agendaitems', 'phases', 'remarks'];

export const addByConfidentiality = async (confidentiality) => {
    const query = `
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#> 
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
      PREFIX dbpedia: <http://dbpedia.org/ontology/>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#> 
          
      INSERT  { 
         GRAPH <http://mu.semte.ch/graphs/public> {
                ?subcase a dbpedia:UnitOfWork .
                ?subcase mu:uuid ?uuid .
                ?subcase besluitvorming:vertrouwelijkheid ?confidential .

                ?subcase dct:alternative ?short_title . 
                ?subcase dct:title ?title . 
                ?subcase ext:isProcedurestapGearchiveerd ?is_archived . 
                ?subcase besluitvorming:formeelOK ?formally_ok . 
                ?subcase dct:created ?created . 
                ?subcase besluitvorming:besloten ?concluded . 
                ?subcase ext:wordtGetoondAlsMededeling ?show_as_remark . 
                ?subcase ext:procedurestapHeeftBesluit ?decision . 
                ?subcase dct:hasPart ?case . 
                ?subcase besluitvorming:isAangevraagdVoor ?meeting . 
                
                ?subcase ext:procedurestapGoedkeuring ?approval . 
                ?subcase dct:subject ?theme . 
                ?subcase dct:creator ?heeftCreator . 
                ?subcase besluitvorming:heeftBevoegde ?mandatee . 
                ?subcase mandaat:beleidsdomein ?government_domain . 
                ?subcase dct:relation ?relation_to . 
                ?subcase ext:bevatDocumentversie ?document_version . 
                ?subcase ext:procedurestap ?document_identifier . 
                ?subcase ext:bevatConsultatievraag ?consultation_request . 
                ?subcase besluitvorming:isGeagendeerdVia ?agendaitem . 
                ?subcase ext:subcaseProcedurestapFase ?phase . 
                ?subcase besluitvorming:opmerking ?remark . 
         }
        } WHERE { 
           GRAPH <http://mu.semte.ch/application> { 
                ?subcase a dbpedia:UnitOfWork .
                ?subcase mu:uuid ?uuid .
                ?subcase besluitvorming:vertrouwelijkheid ?confidential .
                ?confidential skos:prefLabel ${confidentiality.confidentiality} .
                OPTIONAL { ?subcase dct:alternative ?short_title . }
                OPTIONAL { ?subcase dct:title ?title . }
                OPTIONAL { ?subcase ext:isProcedurestapGearchiveerd ?is_archived . }
                OPTIONAL { ?subcase besluitvorming:formeelOK ?formally_ok . }
                OPTIONAL { ?subcase dct:created ?created . }
                OPTIONAL { ?subcase besluitvorming:besloten ?concluded . }
                OPTIONAL { ?subcase ext:wordtGetoondAlsMededeling ?show_as_remark . }
                OPTIONAL { ?subcase ext:procedurestapHeeftBesluit ?decision . }
                OPTIONAL { ?subcase dct:hasPart ?case . }
                OPTIONAL { ?subcase besluitvorming:isAangevraagdVoor ?meeting . }
                
                OPTIONAL { ?subcase ext:procedurestapGoedkeuring ?approval . }
                OPTIONAL { ?subcase dct:subject ?theme . }
                OPTIONAL { ?subcase dct:creator ?heeftCreator . }
                OPTIONAL { ?subcase besluitvorming:heeftBevoegde ?mandatee . }
                OPTIONAL { ?subcase mandaat:beleidsdomein ?government_domain . }
                OPTIONAL { ?subcase dct:relation ?relation_to . }
                OPTIONAL { ?subcase ext:bevatDocumentversie ?document_version . }
                OPTIONAL { ?subcase ext:procedurestap ?document_identifier . }
                OPTIONAL { ?subcase ext:bevatConsultatievraag ?consultation_request . }
                OPTIONAL { ?subcase besluitvorming:isGeagendeerdVia ?agendaitem . }
                OPTIONAL { ?subcase ext:subcaseProcedurestapFase ?phase . }
                OPTIONAL { ?subcase besluitvorming:opmerking ?remark . }
            }
        }`;
    let data = await mu.query(query);
    return parseSparQlResults(data, has_many);
};

export const removeByConfidentiality = async (confidentiality) => {
    const query = `
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#> 
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
      PREFIX dbpedia: <http://dbpedia.org/ontology/>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#> 
      
      DELETE  { 
         GRAPH <http://mu.semte.ch/application> {
                ?subcase a dbpedia:UnitOfWork .
                ?subcase mu:uuid ?uuid .
                ?subcase besluitvorming:vertrouwelijkheid ?confidential .

                ?subcase dct:alternative ?short_title . 
                ?subcase dct:title ?title . 
                ?subcase ext:isProcedurestapGearchiveerd ?is_archived . 
                ?subcase besluitvorming:formeelOK ?formally_ok . 
                ?subcase dct:created ?created . 
                ?subcase besluitvorming:besloten ?concluded . 
                ?subcase ext:wordtGetoondAlsMededeling ?show_as_remark . 
                ?subcase ext:procedurestapHeeftBesluit ?decision . 
                ?subcase dct:hasPart ?case . 
                ?subcase besluitvorming:isAangevraagdVoor ?meeting . 
                
                ?subcase ext:procedurestapGoedkeuring ?approval . 
                ?subcase dct:subject ?theme . 
                ?subcase dct:creator ?heeftCreator . 
                ?subcase besluitvorming:heeftBevoegde ?mandatee . 
                ?subcase mandaat:beleidsdomein ?government_domain . 
                ?subcase dct:relation ?relation_to . 
                ?subcase ext:bevatDocumentversie ?document_version . 
                ?subcase ext:procedurestap ?document_identifier . 
                ?subcase ext:bevatConsultatievraag ?consultation_request . 
                ?subcase besluitvorming:isGeagendeerdVia ?agendaitem . 
                ?subcase ext:subcaseProcedurestapFase ?phase . 
                ?subcase besluitvorming:opmerking ?remark . 
         }
        } WHERE { 
           GRAPH <http://mu.semte.ch/application> { 
                ?subcase a dbpedia:UnitOfWork .
                ?subcase mu:uuid ?uuid .
                ?subcase besluitvorming:vertrouwelijkheid ?confidential .
                ?confidential skos:prefLabel ${confidentiality.confidentiality} .
                OPTIONAL { ?subcase dct:alternative ?short_title . }
                OPTIONAL { ?subcase dct:title ?title . }
                OPTIONAL { ?subcase ext:isProcedurestapGearchiveerd ?is_archived . }
                OPTIONAL { ?subcase besluitvorming:formeelOK ?formally_ok . }
                OPTIONAL { ?subcase dct:created ?created . }
                OPTIONAL { ?subcase besluitvorming:besloten ?concluded . }
                OPTIONAL { ?subcase ext:wordtGetoondAlsMededeling ?show_as_remark . }
                OPTIONAL { ?subcase ext:procedurestapHeeftBesluit ?decision . }
                OPTIONAL { ?subcase dct:hasPart ?case . }
                OPTIONAL { ?subcase besluitvorming:isAangevraagdVoor ?meeting . }
                
                OPTIONAL { ?subcase ext:procedurestapGoedkeuring ?approval . }
                OPTIONAL { ?subcase dct:subject ?theme . }
                OPTIONAL { ?subcase dct:creator ?heeftCreator . }
                OPTIONAL { ?subcase besluitvorming:heeftBevoegde ?mandatee . }
                OPTIONAL { ?subcase mandaat:beleidsdomein ?government_domain . }
                OPTIONAL { ?subcase dct:relation ?relation_to . }
                OPTIONAL { ?subcase ext:bevatDocumentversie ?document_version . }
                OPTIONAL { ?subcase ext:procedurestap ?document_identifier . }
                OPTIONAL { ?subcase ext:bevatConsultatievraag ?consultation_request . }
                OPTIONAL { ?subcase besluitvorming:isGeagendeerdVia ?agendaitem . }
                OPTIONAL { ?subcase ext:subcaseProcedurestapFase ?phase . }
                OPTIONAL { ?subcase besluitvorming:opmerking ?remark . }
              } 
            }`;
    let data = await mu.query(query);
    return parseSparQlResults(data, has_many);
};
