#!/bin/bash

echo 'this script is for reference only';
echo 'run query if decision-activities do not have a subcase cfr. KAS-1942';
echo 'updated query after refactor treatment/decisionActivity'
echo 'then restart cache and resource';

exit;

PREFIX dct: <http://purl.org/dc/terms/>
PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
PREFIX besluitvorming: <https://data.vlaanderen.be/ns/besluitvorming#>
PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
INSERT {
  GRAPH ?G  {
    ?decisionActivity ext:beslissingVindtPlaatsTijdens ?subcase .
  }
} WHERE {
  GRAPH ?G  {
    ?agenda a besluitvorming:Agenda ;
            mu:uuid "AGENDA_ID" ; THIS IS OPTIONAL !
            dct:hasPart ?agendaitem .
    ?agendaitem a besluit:Agendapunt .
    ?subcase ^besluitvorming:vindtPlaatsTijdens / besluitvorming:genereertAgendapunt ?agendaitem .
    ?treatment besluitvorming:heeftOnderwerp ?agendaitem .
    ?treatment besluitvorming:heeftBeslissing ?decisionActivity .
    FILTER NOT EXISTS { ?decisionActivity ext:beslissingVindtPlaatsTijdens ?subcase . }
  }
}