import mu from 'mu';
import { parseSparQlResults } from './helpers';

const has_many = ['document_versions',  'remarks'];

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
                ?document a foaf:Document .
                ?document mu:uuid ?uuid . 
                ?document besluitvorming:vertrouwelijkheid ?confidential .

                ?document besluitvorming:gearchiveerd ?archived . 
                ?document dct:title ?title . 
                ?document ext:omschrijving ?description . 
                ?document dct:created ?created . 
                ?document besluitvorming:stuknummerVP ?number_vp . 
                ?document besluitvorming:stuknummerVR ?number_vr . 
                ?document besluitvorming:opmerking ?remark .
                ?document besluitvorming:heeftVersie ?document_version . 
                ?document ext:besluitHeeftDocument ?decision . 
                ?document ext:documentType ?type . 
         }
        } WHERE { 
           GRAPH <http://mu.semte.ch/application> { 
                ?document a foaf:Document .
                OPTIONAL { ?document mu:uuid ?uuid . }
                ?document besluitvorming:vertrouwelijkheid ?confidential .
                ?confidential skos:prefLabel ${confidentiality.confidentiality} .
                OPTIONAL { ?document besluitvorming:gearchiveerd ?archived . }
                OPTIONAL { ?document dct:title ?title . }
                OPTIONAL { ?document ext:omschrijving ?description . }
                OPTIONAL { ?document dct:created ?created . }
                OPTIONAL { ?document besluitvorming:stuknummerVP ?number_vp . }
                OPTIONAL { ?document besluitvorming:stuknummerVR ?number_vr . }
                OPTIONAL { ?document besluitvorming:opmerking ?remark . }
                OPTIONAL { ?document besluitvorming:heeftVersie ?document_version . }
                OPTIONAL { ?document ext:besluitHeeftDocument ?decision . }
                OPTIONAL { ?document ext:documentType ?type . }
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
                ?document a foaf:Document .
                ?document mu:uuid ?uuid . 
                ?document besluitvorming:vertrouwelijkheid ?confidential .
                ?confidential skos:prefLabel ${confidentiality.confidentiality} .
                ?document besluitvorming:gearchiveerd ?archived . 
                ?document dct:title ?title . 
                ?document ext:omschrijving ?description . 
                ?document dct:created ?created . 
                ?document besluitvorming:stuknummerVP ?number_vp . 
                ?document besluitvorming:stuknummerVR ?number_vr . 
                ?document besluitvorming:opmerking ?remark .
                ?document besluitvorming:heeftVersie ?document_version . 
                ?document ext:besluitHeeftDocument ?decision . 
                ?document ext:documentType ?type . 
         }
        } WHERE { 
           GRAPH <http://mu.semte.ch/application> { 
                ?document a foaf:Document .
                OPTIONAL { ?document mu:uuid ?uuid . }
                ?document besluitvorming:vertrouwelijkheid ?confidential .
                ?confidential skos:prefLabel "Vertrouwelijk"@nl .
                OPTIONAL { ?document besluitvorming:gearchiveerd ?archived . }
                OPTIONAL { ?document dct:title ?title . }
                OPTIONAL { ?document ext:omschrijving ?description . }
                OPTIONAL { ?document dct:created ?created . }
                OPTIONAL { ?document besluitvorming:stuknummerVP ?number_vp . }
                OPTIONAL { ?document besluitvorming:stuknummerVR ?number_vr . }
                OPTIONAL { ?document besluitvorming:opmerking ?remark . }
                OPTIONAL { ?document besluitvorming:heeftVersie ?document_version . }
                OPTIONAL { ?document ext:besluitHeeftDocument ?decision . }
                OPTIONAL { ?document ext:documentType ?type . }
              } 
            }`;
    let data = await mu.query(query);
    return parseSparQlResults(data, has_many);
};

/*
const getDocumentProps = (item) => {
    const uri = `<Document:${item.uuid}>`;
    let update_obj = '';
    update_obj += item.archived ? `${uri} besluitvorming:gearchiveerd "${item.archived}" .\n` : '';
    update_obj += item.title ? `${uri} dct:title "${item.title}" .\n` : '';
    update_obj += item.description ? `${uri} ext:omschrijving "${item.description}" .\n` : '';
    update_obj += item.created ? `${uri} dct:created "${item.created}" .\n` : '';
    update_obj += item.number_vp ? `${uri} besluitvorming:stuknummerVP "${item.number_vp}" .\n` : '';
    update_obj += item.number_vr ? `${uri} besluitvorming:stuknummerVR "${item.number_vr}" .\n` : '';
    update_obj += item.archived ? `${uri} ext:besluitHeeftDocument "${item.decision}" .\n` : '';
    update_obj += item.type ? `${uri} ext:documentType ${item.type}" .\n` : '';
    has_many.forEach(relation => {
        const predicate = getPredicate(relation);
        const property = item[relation] || [];
        property.map(value => {
            update_obj += `${uri} ${predicate} ${value} .\n`
        })
    });
    return update_obj;
};

const getPredicate = (relation) => {
    if (relation === 'remarks'){
        return 'besluitvorming:opmerking';
    }else if (relation === 'document_versions'){
        return 'besluitvorming:heeftVersie';
    }
};

*/


