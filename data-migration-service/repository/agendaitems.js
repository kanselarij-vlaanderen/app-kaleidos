import mu from 'mu';
import { parseSparQlResults } from './helpers';

const has_many = ['approvals', 'themes', 'creators', 'mandatees', 'government_domains', 'relates_to', 'document_versions', 'document_identifiers',
    'consultation_requests', 'agendaitems', 'phases', 'remarks'];

export const addByConfidentiality = async (confidentiality = {}) => {
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
                ?agendaitem a besluit:Agendapunt .
                ?agendaitem mu:uuid ?uuid .
                ?agendaitem besluitvorming:vertrouwelijkheid ?confidential .
                ?agendaitem dct:alternative ?short_title . 
                ?agendaitem ext:forPress ?for_press . 
                ?agendaitem ext:prioriteit ?priority . 
                ?agendaitem besluitvorming:formeelOK ?formally_ok . 
                ?agendaitem dct:created ?created . 
                ?agendaitem besluitvorming:notulen ?record . 
                ?agendaitem besluitvorming:titelPersagenda ?title_press . 
                ?agendaitem besluitvorming:tekstPersagenda ?text_press . 
                ?agendaitem dct:title ?title . 
                ?agendaitem ext:wordtGetoondAlsMededeling ?show_as_remark . 
                
                ?agendaitem ext:heeftVerdaagd ?postponed . 
                ?agendaitem besluit:aangebrachtNa ?previous_agendaitem . 
                ?agendaitem besluitvorming:isGeagendeerdVia ?subcase . 
                ?agendaitem ext:agendapuntHeeftBesluit ?decision . 
                ?agendaitem dct:hasPart ?agenda . 
                ?agendaitem ext:nieuwsbriefInfo ?newsletter_info . 
                ?agendaitem ext:notulenVanAgendaPunt ?meeting_record . 

                ?agendaitem besluit:heeftAanwezige ?attendee . 
                ?agendaitem besluitvorming:opmerking ?remark . 
                ?agendaitem ext:agendapuntGoedkeuring ?approval . 
                ?agendaitem dct:agendapuntSubject ?theme . 
                ?agendaitem besluitvorming:heeftBevoegdeVoorAgendapunt ?mandatee . 
                ?agendaitem mandaat:agendapuntBeleidsdomein ?government_domain . 
                ?agendaitem ext:bevatAgendapuntDocumentversie ?document_version . 
                ?agendaitem ext:subcaseAgendapuntFase ?phase . 
         }
        } WHERE { 
           GRAPH <http://mu.semte.ch/application> { 
                ?agendaitem a besluit:Agendapunt .
                ?agendaitem mu:uuid ?uuid .
                ?agendaitem besluitvorming:vertrouwelijkheid ?confidential .
                ?confidential skos:prefLabel ${confidentiality.confidentiality} .
                OPTIONAL { ?agendaitem dct:alternative ?short_title .  }
                OPTIONAL { ?agendaitem ext:forPress ?for_press .  }
                OPTIONAL { ?agendaitem ext:prioriteit ?priority .  }
                OPTIONAL { ?agendaitem besluitvorming:formeelOK ?formally_ok .  }
                OPTIONAL { ?agendaitem dct:created ?created .  }
                OPTIONAL { ?agendaitem besluitvorming:notulen ?record .  }
                OPTIONAL { ?agendaitem besluitvorming:titelPersagenda ?title_press .  }
                OPTIONAL { ?agendaitem besluitvorming:tekstPersagenda ?text_press .  }
                OPTIONAL { ?agendaitem dct:title ?title .  }
                OPTIONAL { ?agendaitem ext:wordtGetoondAlsMededeling ?show_as_remark .  }
                
                OPTIONAL { ?agendaitem ext:heeftVerdaagd ?postponed . }
                OPTIONAL { ?agendaitem besluit:aangebrachtNa ?previous_agendaitem .  }
                OPTIONAL { ?agendaitem besluitvorming:isGeagendeerdVia ?subcase .  }
                OPTIONAL { ?agendaitem ext:agendapuntHeeftBesluit ?decision .  }
                OPTIONAL { ?agendaitem dct:hasPart ?agenda .  }
                OPTIONAL { ?agendaitem ext:nieuwsbriefInfo ?newsletter_info .  }
                OPTIONAL { ?agendaitem ext:notulenVanAgendaPunt ?meeting_record . } 

                OPTIONAL { ?agendaitem besluit:heeftAanwezige ?attendee . }
                OPTIONAL { ?agendaitem besluitvorming:opmerking ?remark .  }
                OPTIONAL { ?agendaitem ext:agendapuntGoedkeuring ?approval .  }
                OPTIONAL { ?agendaitem dct:agendapuntSubject ?theme . }
                OPTIONAL { ?agendaitem besluitvorming:heeftBevoegdeVoorAgendapunt ?mandatee . } 
                OPTIONAL { ?agendaitem mandaat:agendapuntBeleidsdomein ?government_domain . } 
                OPTIONAL { ?agendaitem ext:bevatAgendapuntDocumentversie ?document_version . }
                OPTIONAL { ?agendaitem ext:subcaseAgendapuntFase ?phase . }
            }
      }`;
    let data = await mu.query(query);
    return parseSparQlResults(data, has_many);
};

export const removeByConfidentiality = async (confidentiality = {}) => {
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
                ?agendaitem a besluit:Agendapunt .
                ?agendaitem mu:uuid ?uuid .
                ?agendaitem besluitvorming:vertrouwelijkheid ?confidential .
                ?agendaitem dct:alternative ?short_title . 
                ?agendaitem ext:forPress ?for_press . 
                ?agendaitem ext:prioriteit ?priority . 
                ?agendaitem besluitvorming:formeelOK ?formally_ok . 
                ?agendaitem dct:created ?created . 
                ?agendaitem besluitvorming:notulen ?record . 
                ?agendaitem besluitvorming:titelPersagenda ?title_press . 
                ?agendaitem besluitvorming:tekstPersagenda ?text_press . 
                ?agendaitem dct:title ?title . 
                ?agendaitem ext:wordtGetoondAlsMededeling ?show_as_remark . 
                
                ?agendaitem ext:heeftVerdaagd ?postponed . 
                ?agendaitem besluit:aangebrachtNa ?previous_agendaitem . 
                ?agendaitem besluitvorming:isGeagendeerdVia ?subcase . 
                ?agendaitem ext:agendapuntHeeftBesluit ?decision . 
                ?agendaitem dct:hasPart ?agenda . 
                ?agendaitem ext:nieuwsbriefInfo ?newsletter_info . 
                ?agendaitem ext:notulenVanAgendaPunt ?meeting_record . 

                ?agendaitem besluit:heeftAanwezige ?attendee . 
                ?agendaitem besluitvorming:opmerking ?remark . 
                ?agendaitem ext:agendapuntGoedkeuring ?approval . 
                ?agendaitem dct:agendapuntSubject ?theme . 
                ?agendaitem besluitvorming:heeftBevoegdeVoorAgendapunt ?mandatee . 
                ?agendaitem mandaat:agendapuntBeleidsdomein ?government_domain . 
                ?agendaitem ext:bevatAgendapuntDocumentversie ?document_version . 
                ?agendaitem ext:subcaseAgendapuntFase ?phase . 
         }
        } WHERE { 
           GRAPH <http://mu.semte.ch/application> { 
                ?agendaitem a besluit:Agendapunt .
                ?agendaitem mu:uuid ?uuid .
                ?agendaitem besluitvorming:vertrouwelijkheid ?confidential .
                ?confidential skos:prefLabel ${confidentiality.confidentiality} .
                OPTIONAL { ?agendaitem dct:alternative ?short_title .  }
                OPTIONAL { ?agendaitem ext:forPress ?for_press .  }
                OPTIONAL { ?agendaitem ext:prioriteit ?priority .  }
                OPTIONAL { ?agendaitem besluitvorming:formeelOK ?formally_ok .  }
                OPTIONAL { ?agendaitem dct:created ?created .  }
                OPTIONAL { ?agendaitem besluitvorming:notulen ?record .  }
                OPTIONAL { ?agendaitem besluitvorming:titelPersagenda ?title_press .  }
                OPTIONAL { ?agendaitem besluitvorming:tekstPersagenda ?text_press .  }
                OPTIONAL { ?agendaitem dct:title ?title .  }
                OPTIONAL { ?agendaitem ext:wordtGetoondAlsMededeling ?show_as_remark .  }
                
                OPTIONAL { ?agendaitem ext:heeftVerdaagd ?postponed . }
                OPTIONAL { ?agendaitem besluit:aangebrachtNa ?previous_agendaitem .  }
                OPTIONAL { ?agendaitem besluitvorming:isGeagendeerdVia ?subcase .  }
                OPTIONAL { ?agendaitem ext:agendapuntHeeftBesluit ?decision .  }
                OPTIONAL { ?agendaitem dct:hasPart ?agenda .  }
                OPTIONAL { ?agendaitem ext:nieuwsbriefInfo ?newsletter_info .  }
                OPTIONAL { ?agendaitem ext:notulenVanAgendaPunt ?meeting_record . } 

                OPTIONAL { ?agendaitem besluit:heeftAanwezige ?attendee . }
                OPTIONAL { ?agendaitem besluitvorming:opmerking ?remark .  }
                OPTIONAL { ?agendaitem ext:agendapuntGoedkeuring ?approval .  }
                OPTIONAL { ?agendaitem dct:agendapuntSubject ?theme . }
                OPTIONAL { ?agendaitem besluitvorming:heeftBevoegdeVoorAgendapunt ?mandatee . } 
                OPTIONAL { ?agendaitem mandaat:agendapuntBeleidsdomein ?government_domain . } 
                OPTIONAL { ?agendaitem ext:bevatAgendapuntDocumentversie ?document_version . }
                OPTIONAL { ?agendaitem ext:subcaseAgendapuntFase ?phase . }
              } 
            }`;
    let data = await mu.query(query);
    return parseSparQlResults(data, has_many);
};


/*

const getAgendaItemProps = (item) => {
    const uri = `<Document:${item.uuid}>`;
    let update_obj = '';
    update_obj += item.short_title ? `${uri} besluitvorming:gearchiveerd "${item.short_title}" .\n` : '';
    update_obj += item.for_press ? `${uri} dct:title "${item.for_press}" .\n` : '';
    update_obj += item.priority ? `${uri} ext:omschrijving "${item.priority}" .\n` : '';
    update_obj += item.formally_ok ? `${uri} dct:created "${item.formally_ok}" .\n` : '';
    update_obj += item.created ? `${uri} besluitvorming:stuknummerVP "${item.created}" .\n` : '';
    update_obj += item.record ? `${uri} besluitvorming:stuknummerVR "${item.record}" .\n` : '';
    update_obj += item.title_press ? `${uri} ext:besluitHeeftDocument "${item.title_press}" .\n` : '';
    update_obj += item.text_press ? `${uri} ext:documentType ${item.text_press}" .\n` : '';
    update_obj += item.title ? `${uri} ext:documentType ${item.title}" .\n` : '';
    update_obj += item.show_as_remark ? `${uri} ext:documentType ${item.show_as_remark}" .\n` : '';

    update_obj += item.postponed ? `${uri} ext:documentType ${item.postponed}" .\n` : '';
    update_obj += item.previous_agendaitem ? `${uri} ext:documentType ${item.previous_agendaitem}" .\n` : '';
    update_obj += item.subcase ? `${uri} ext:documentType ${item.subcase}" .\n` : '';
    update_obj += item.decision ? `${uri} ext:documentType ${item.decision}" .\n` : '';
    update_obj += item.agenda ? `${uri} ext:documentType ${item.agenda}" .\n` : '';
    update_obj += item.newsletter_info ? `${uri} ext:documentType ${item.newsletter_info}" .\n` : '';
    update_obj += item.meeting_record ? `${uri} ext:documentType ${item.meeting_record}" .\n` : '';

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
    if (relation === 'attendees'){
        return 'besluit:heeftAanwezige';
    }else if (relation === 'remarks'){
        return 'besluitvorming:opmerking';
    }else if (relation === 'approvals'){
        return 'ext:agendapuntGoedkeuring';
    }else if (relation === 'themes'){
        return 'dct:agendapuntSubject';
    }else if (relation === 'mandatees'){
        return 'besluitvorming:heeftBevoegdeVoorAgendapunt';
    }else if (relation === 'government_domains'){
        return 'mandaat:agendapuntBeleidsdomein';
    }else if (relation === 'document_versions'){
        return 'ext:bevatAgendapuntDocumentversie';
    }else if (relation === 'phases'){
        return 'ext:subcaseAgendapuntFase';
    }
};


 */
