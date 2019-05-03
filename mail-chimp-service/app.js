import mu from 'mu';
import { ok } from 'assert';

const app = mu.app;
const bodyParser = require('body-parser');
const repository = require('./repository');
const cors = require('cors');

app.use(bodyParser.json({ type: 'application/*+json' }));
app.use(cors());


app.get('/', (req, res) => {
  return setNewsletter(req, res);
});

app.post('/', (req, res) => {
  return setNewsletter(req, res);
});


const setNewsletter = async (req, res) => {

    try {

      const agendaId = req.query.agendaId;

      if (!agendaId){
        throw new Error("alles kaputski");
      }

      const newsletter = await repository.getNewsletterInfo(agendaId);

      res.send({ newsletter })



    }catch(error) {
        console.error(error);
        res.send({ status: ok, statusCode: 500, body: { error } });
    }
};
