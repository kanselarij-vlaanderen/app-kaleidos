import mu from 'mu';
import { ok } from 'assert';

const app = mu.app;
const bodyParser = require('body-parser');
const repository = require('./repository');
const cors = require('cors');

app.use(bodyParser.json({ type: 'application/*+json' }));
app.use(cors());


app.post('/', (req, res) => {
  return handleCloseMeetingRequest(req, res);
});

app.get('/', (req, res) => {
    return handleCloseMeetingRequest(req, res);
  });

const handleCloseMeetingRequest = async (req, res) => {

    let agendaId = req.query.agendaId;
    try {

      const originals = await repository.getRelatedSubCasesOfAgenda(agendaId);
      let { not_decided, decided, meeting } = await seperateSubcasesWhenPostponed(originals);

      const agendaItems = not_decided.map(item => {
        return { uri : item.agendapunt }
      });

      const postponedAgendaItems = await repository.retractAgendaItems(agendaItems);
      const closed_subcases = await repository.concludeSubCases(not_decided);

      await repository.finaliseMeeting(meeting);

      const updated_subcases = await repository.getRelatedSubCasesOfAgenda(agendaId);
      res.send({ status: ok, statusCode: 200, body: { originals, updated_subcases } });
    }catch(error) {
        console.error(error);
        res.send({ status: ok, statusCode: 500, body: { error } });
    }
};

const seperateSubcasesWhenPostponed = async (subcases) => {
  const meeting = subcases[0].meeting;
  const not_decided = subcases.filter(item => !item.decision);
  const decided = subcases.filter(item => item.decision);
  return { meeting, not_decided, decided };
}
