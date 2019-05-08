import mu from 'mu';
import moment from 'moment';
import { removeInfoNotInTemp, notConfidentialFilter, addRelatedFiles, cleanup, fillOutDetailsOnVisibleItems, addRelatedToAgendaItemAndSubcase } from './helpers';

const tempGraph = `http://mu.semte.ch/temp/${mu.uuid()}`;
//const adminGraph = `http://mu.semte.ch/graphs/organizations/kanselarij`;
const adminGraph = `http://mu.semte.ch/application`;
const targetGraph = `http://mu.semte.ch/graphs/organizations/user`;

const addVisibleAgendas = () => {
  // TODO can reduce the number of agendas examined using delta service
  const query = `
  PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
  PREFIX dct: <http://purl.org/dc/terms/>
  PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
  PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
  INSERT {
    GRAPH <${tempGraph}> {
      ?s a besluitvorming:Agenda.
    }
  } where {
    GRAPH <${adminGraph}> {
      ?s a besluitvorming:Agenda.
      ?s ext:agendaNaam ?naam.
      FILTER(?naam != "Ontwerpagenda")
      
      ?s dct:hasPart ?item.
      ?subcase besluitvorming:isGeagendeerdVia ?item.
      ?subcase ext:procedurestapHeeftBesluit ?decision.
      ?decision besluitvorming:goedgekeurd "true"^^<http://mu.semte.ch/vocabularies/typed-literals/boolean> .
    }
  }`;
  return mu.query(query);
};
const addRelatedToAgenda = () => {
  const query = `
  PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
  PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
  PREFIX dct: <http://purl.org/dc/terms/>
  PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
  PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
  INSERT {
    GRAPH <${tempGraph}> {
      ?s a ?thing .
      ?subcase a dbpedia:UnitOfWork .
    }
  } where {
    GRAPH <${tempGraph}> {
      ?agenda a besluitvorming:Agenda .
    }
    GRAPH <${adminGraph}> {
      ?s a ?thing .
      FILTER NOT EXISTS {
        ?s a besluit:AgendaPunt .
        ?subcase besluitvorming:isGeagendeerdVia ?agendaItem .
        ?subcase ext:procedurestapHeeftBesluit ?decision.
        FILTER NOT EXISTS {
          ?decision besluitvorming:goedgekeurd "true"^^<http://mu.semte.ch/vocabularies/typed-literals/boolean> .
        }
      }
      { { ?s ?p ?agenda } UNION { ?agenda ?p ?s } }
      FILTER( ?thing NOT IN(besluitvorming:Agenda) )
      ${notConfidentialFilter}

    }
  }`;
  return mu.query(query);
};

const addRelatedDocuments = () => {
  const query = `
  PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
  PREFIX dct: <http://purl.org/dc/terms/>
  PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
  PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
  PREFIX dbpedia: <http://dbpedia.org/ontology/>
  PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
  PREFIX nfo: <http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  INSERT {
    GRAPH <${tempGraph}> {
      ?s a ?thing .
      ?version a ?subthing .
    }
  } where {
    GRAPH <${tempGraph}> {
      ?target a ?targetClass .
    }
    GRAPH <${adminGraph}> {
      ?s a ?thing .
      ?target a besluit:Besluit .
      ?target besluitvorming:goedgekeurd "true"^^<http://mu.semte.ch/vocabularies/typed-literals/boolean> .
      ?target ?p ?s .

      FILTER( ?thing IN(
        foaf:Document,
        ext:DocumentVersie ) )
      
      ${notConfidentialFilter}

      OPTIONAL {
        ?s besluitvorming:heeftVersie ?version.
        ?version a ?subthing.
      }
    }
  }`;
  return mu.query(query);
};




export const fillUp = async () => {
  const start = moment().utc();
  console.log(`fill overheid started at: ${start}`);
  await addVisibleAgendas();
  await addRelatedToAgenda();
  await addRelatedToAgendaItemAndSubcase(tempGraph, adminGraph);
  await addRelatedDocuments();
  await addRelatedFiles(tempGraph, adminGraph);
  await fillOutDetailsOnVisibleItems(tempGraph, targetGraph, adminGraph);
  await removeInfoNotInTemp(tempGraph, targetGraph);
  await cleanup(tempGraph);
  const end = moment().utc();
  console.log(`fill overheid ended at: ${end}, took: ${end.diff(start, 'ms')}ms`);
};