import mu from 'mu';
import { ok } from 'assert';

const app = mu.app;
const bodyParser = require('body-parser');
const cors = require('cors');
const cron = require('node-cron');

const subcaseRepository = require('./repository/subcases');
const agendaitemRepository = require('./repository/agendaitems');
const documentRepository = require('./repository/documents');

app.use(bodyParser.json({ type: 'application/*+json' }));
app.use(cors());

const confidentialities = [
  {
    confidentiality : '"Vertrouwelijk"@nl',
    graph: 'http://mu.semte.ch/graphs/organizations/kanselarij'
  },
  {
    confidentiality : '"Op aanvraag"@nl',
    graph: 'http://mu.semte.ch/graphs/organizations/requests'
  },
  {
    confidentiality : '"Actief openbaar"@nl',
    graph: 'http://mu.semte.ch/graphs/public'
  },
  {
    confidentiality : '"Beperkt openbaar"@nl',
    graph: 'http://mu.semte.ch/graphs/organizations/requests'
  }
];

cron.schedule('*/5 * * * *', async () => {

  for (let i = 0; i < confidentialities.length; i++) {
    const confidentiality = confidentialities[i];

    await documentRepository.addByConfidentiality(confidentiality);
    await documentRepository.removeByConfidentiality(confidentiality);

    await subcaseRepository.addByConfidentiality(confidentiality);
    await subcaseRepository.removeByConfidentiality(confidentiality);

    await agendaitemRepository.addByConfidentiality(confidentiality);
    await agendaitemRepository.removeByConfidentiality(confidentiality);
  }

});
