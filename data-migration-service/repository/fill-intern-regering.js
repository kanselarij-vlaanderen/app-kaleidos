import mu from 'mu';
import moment from 'moment';
import { removeInfoNotInTemp, addRelatedFiles, cleanup, fillOutDetailsOnVisibleItems, addAllRelatedDocuments, addAllRelatedToAgenda, addRelatedToAgendaItemAndSubcase } from './helpers';

const tempGraph = `http://mu.semte.ch/temp/${mu.uuid()}`;
const adminGraph = `http://mu.semte.ch/graphs/organizations/kanselarij`;
const targetGraph = `http://mu.semte.ch/graphs/organizations/kabinetten`;

const addVisibleAgendas = () => {
  const query = `
  PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
  PREFIX dct: <http://purl.org/dc/terms/>
  PREFIX besluitvorming: <http://data.vlaanderen.be/ns/besluitvorming#>
  PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
  INSERT {
    GRAPH <${tempGraph}> {
      ?s a <http://data.vlaanderen.be/ns/besluitvorming#Agenda>.
    }
  } where {
    GRAPH <${adminGraph}> {
      ?s a <http://data.vlaanderen.be/ns/besluitvorming#Agenda>.
      ?s ext:agendaNaam ?naam.
      FILTER(?naam != "Ontwerpagenda")
    }
  }`;
  return mu.query(query);
};

export const fillUp = async () => {
  const start = moment().utc();
  console.log(`fill regering started at: ${start}`);
  await addVisibleAgendas();
  await addAllRelatedToAgenda(tempGraph, adminGraph);
  await addRelatedToAgendaItemAndSubcase(tempGraph, adminGraph);
  await addAllRelatedDocuments(tempGraph, adminGraph);
  await addRelatedFiles(tempGraph, adminGraph);
  await fillOutDetailsOnVisibleItems(tempGraph, targetGraph, adminGraph);
  await removeInfoNotInTemp(tempGraph, targetGraph);
  await cleanup(tempGraph);
  const end = moment().utc();
  console.log(`fill regering ended at: ${end}, took: ${end.diff(start, 'ms')}ms`);
};