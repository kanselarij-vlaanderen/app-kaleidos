import promClient from 'prom-client';
import { query, sparqlEscapeUri, sparqlEscapeDateTime } from 'mu';

const SUBMISSIONS_GRAPH = 'http://mu.semte.ch/graphs/system/submissions';
const SUBMISSION_STATUS_CONCEPT = 'http://themis.vlaanderen.be/id/indieningstatus/0732c914-3daf-454b-8117-1486ccd3b6e2';
// A submission gets its status ("Ingediend", ...) a few seconds before the
// agenda-submission-service links it to a meeting. Only submissions that were
// last modified longer ago than this grace period are counted.
const SUBMISSION_GRACE_PERIOD_MINUTES = 15;

new promClient.Gauge({
  name: 'submissions_without_meeting',
  help: 'Number of non-concept submissions that are not linked to an existing meeting',
  labelNames: ['data_validation', 'data_validation_scope'],

  async collect() {
    const modifiedBefore = new Date(Date.now() - SUBMISSION_GRACE_PERIOD_MINUTES * 60 * 1000);
    const result = await query(`
    PREFIX subm: <http://mu.semte.ch/vocabularies/ext/submissions/>
    PREFIX adms: <http://www.w3.org/ns/adms#>
    PREFIX dct: <http://purl.org/dc/terms/>
    PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
    PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

    SELECT (COUNT(DISTINCT ?submission) as ?count)
    WHERE {
      GRAPH ${sparqlEscapeUri(SUBMISSIONS_GRAPH)} {
        ?submission a subm:Indiening .
        OPTIONAL { ?submission dct:created ?created . }
        OPTIONAL { ?submission ext:modified ?modified . }
        OPTIONAL { ?submission adms:status ?status . }
        OPTIONAL { ?submission subm:ingediendVoorVergadering ?meeting . }
      }
      FILTER (!BOUND(?status) || ?status != ${sparqlEscapeUri(SUBMISSION_STATUS_CONCEPT)})
      FILTER (!BOUND(?meeting) || NOT EXISTS { GRAPH ?meetingGraph { ?meeting a besluit:Vergaderactiviteit . } })
      FILTER (COALESCE(?modified, ?created, "1970-01-01T00:00:00Z"^^xsd:dateTime) < ${sparqlEscapeDateTime(modifiedBefore)})
    }`, { sudo: true });
    const count = parseInt(result.results.bindings[0]['count'].value);
    this.labels({ data_validation: 1, data_validation_scope: 'submissions' }).set(count);
  }
});

export default [];
