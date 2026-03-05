import promClient from 'prom-client';
import { query, sparqlEscapeUri } from 'mu';

new promClient.Gauge({
  name: 'missing_document_id',
  help: 'Number of documents without document id',
  labelNames: ['data_validation', 'data_validation_scope'],

  async collect() {
    const result = await query(`
    PREFIX dossier: <https://data.vlaanderen.be/ns/dossier#>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>

    SELECT (COUNT(?piece) as ?count)
    FROM <http://mu.semte.ch/graphs/organizations/kanselarij>
    WHERE {
      ?documentContainer a dossier:Serie .
      ?documentContainer dossier:Collectie.bestaatUit ?piece .
      FILTER NOT EXISTS { ?piece mu:uuid ?id . }
    }`, { sudo: true });
    const count = parseInt(result.results.bindings[0]['count'].value);
    this.labels({ data_validation: 1, data_validation_scope: 'documents'}).set(count);
  }
});

new promClient.Gauge({
  name: 'missing_document_type',
  help: 'Number of documents without document types',
  labelNames: ['data_validation', 'data_validation_scope'],

  async collect() {
    const result = await query(`
    PREFIX dossier: <https://data.vlaanderen.be/ns/dossier#>

    SELECT (COUNT(?piece) as ?count)
    FROM <http://mu.semte.ch/graphs/organizations/kanselarij>
    WHERE {
      ?documentContainer a dossier:Serie .
      ?documentContainer dossier:Collectie.bestaatUit ?piece .
      FILTER NOT EXISTS { ?piece a ?type . }
    }`, { sudo: true });
    const count = parseInt(result.results.bindings[0]['count'].value);
    this.labels({ data_validation: 1, data_validation_scope: 'documents'}).set(count);
  }
});

new promClient.Gauge({
  name: 'document_counts',
  help: 'Number of documents per graph',
  labelNames: ['data_validation', 'data_validation_scope', 'graph'],

  async collect() {
    const graphs = [
      'http://mu.semte.ch/graphs/organizations/kanselarij',
      'http://mu.semte.ch/graphs/organizations/minister',
      'http://mu.semte.ch/graphs/organizations/intern-regering',
      'http://mu.semte.ch/graphs/organizations/intern-overheid',
    ];
    for (let graph of graphs) {
      const result = await query(`
        PREFIX dossier: <https://data.vlaanderen.be/ns/dossier#>
        PREFIX mu: <http://mu.semte.ch/vocabularies/core/>

        SELECT (COUNT(?piece) as ?count)
        FROM ${sparqlEscapeUri(graph)}
        WHERE {
          ?piece a dossier:Stuk .
        }`, { sudo: true });
      const count = parseInt(result.results.bindings[0]['count'].value);
      this.labels({ data_validation: 1, data_validation_scope: 'documents', graph }).set(count);
    }
  }
});

export default [];
