import promClient from 'prom-client';
import { query, sparqlEscapeUri } from 'mu';

const EMAIL_GRAPH = 'http://mu.semte.ch/graphs/system/email';

const EMAIL_FAILBOX = 'http://themis.vlaanderen.be/id/mail-folders/3b9836fa-806a-413e-9492-889ac00cc412';

new promClient.Gauge({
  name: 'failed_emails',
  help: 'Number of e-mails in the failbox of the deliver-email-service',
  labelNames: ['data_validation', 'data_validation_scope'],

  async collect() {
    const result = await query(`
    PREFIX nmo: <http://www.semanticdesktop.org/ontologies/2007/03/22/nmo#>

    SELECT (COUNT(DISTINCT ?email) as ?count)
    FROM ${sparqlEscapeUri(EMAIL_GRAPH)}
    WHERE {
      ?email a nmo:Email ;
        nmo:isPartOf ${sparqlEscapeUri(EMAIL_FAILBOX)} .
    }`, { sudo: true });
    const count = parseInt(result.results.bindings[0]['count'].value);
    this.labels({ data_validation: 1, data_validation_scope: 'emails' }).set(count);
  }
});

export default [];
