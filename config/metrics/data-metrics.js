import promClient from 'prom-client';
import { querySudo as query } from '@lblod/mu-auth-sudo';
const missingTypeGauge = new promClient.Gauge({
  name: 'data_missing_type_count',
  help: 'count of resources that miss rdf:type',
});
const missingIDGauge = new promClient.Gauge({
  name: 'data_missing_id_count',
  help: 'count of resources that miss mu:uuid',
});
const piecesWithoutTypeGauge = new promClient.Gauge({
  name: 'data_pieces_missing_type_count',
  help: 'count of resources that miss rdf:type',
});

const register = promClient.register;


const missingTypeQuery = `
PREFIX mu: <http://mu.semte.ch/vocabularies/core/>

SELECT (COUNT(?resource) as ?resources)
FROM <http://mu.semte.ch/graphs/organizations/kanselarij>
FROM <http://mu.semte.ch/graphs/organizations/intern-regering>
FROM <http://mu.semte.ch/graphs/organizations/intern-overheid>
FROM <http://mu.semte.ch/graphs/organizations/minister>
FROM <http://mu.semte.ch/graphs/system/submissions>
FROM <http://mu.semte.ch/graphs/system/signing>
WHERE {
    ?resource mu:uuid ?uuid.
    FILTER NOT EXISTS { ?resource a ?type . }
}

`
const missingIDQuery = `
PREFIX mu: <http://mu.semte.ch/vocabularies/core/>

SELECT (COUNT(?resource) as ?resources)
FROM <http://mu.semte.ch/graphs/organizations/kanselarij>
FROM <http://mu.semte.ch/graphs/organizations/intern-regering>
FROM <http://mu.semte.ch/graphs/organizations/intern-overheid>
FROM <http://mu.semte.ch/graphs/organizations/minister>
FROM <http://mu.semte.ch/graphs/system/submissions>
FROM <http://mu.semte.ch/graphs/system/signing>
WHERE {
    ?resource a ?type.
    FILTER NOT EXISTS { ?resource mu:uuid ?uuid . }
    # FILTER (?type != <http://mu.semte.ch/vocabularies/ext/TempGraph>)
}

`

const piecesWithoutTypeQuery = `
PREFIX dossier: <https://data.vlaanderen.be/ns/dossier#>

SELECT (COUNT(?piece) as ?pieces) 
FROM <http://mu.semte.ch/graphs/organizations/kanselarij>
where {
?documentContainer a dossier:Serie .
     ?documentContainer dossier:Collectie.bestaatUit ?piece  .
FILTER EXISTS { ?piece a ?type . }

} ORDER BY ?piece
`

async function countResourcesWithoutType() {
  const response = await query(missingTypeQuery);
  if (response.results.bindings) {
    const binding = response.results.bindings[0];
    const count = parseInt(binding.resources.value);
    missingTypeGauge.set(count);
  }
}

async function countResourcesWithoutID() {
  const response = await query(missingIDQuery);
  if (response.results.bindings) {
    const binding = response.results.bindings[0];
    const count = parseInt(binding.resources.value);
    missingIDGauge.set(count);
  }
}

async function countPiecesWithoutID() {
  const response = await query(piecesWithoutTypeQuery);
  if (response.results.bindings) {
    const binding = response.results.bindings[0];
    const count = parseInt(binding.resources.value);
    piecesWithoutTypeGauge.set(count);
  }
}

export default {
  name: 'data checks',
  cronPattern: '*/20 * * * * *',
  async cronExecute() {
    await countResourcesWithoutType();
    await countResourcesWithoutID();
    await countPiecesWithoutID();
  },
  async metrics() {
    // not returning anything
  }
}