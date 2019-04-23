import mu from 'mu';
import { ok } from 'assert';

const app = mu.app;
const bodyParser = require('body-parser');
const repository = require('./repository');
const cors = require('cors');

app.use(bodyParser.json({ type: 'application/*+json' }));
app.use(cors());


app.post('/migrate', (req, res) => {
  return migrateGraph(req, res);
});


const migrateGraph = async (req, res) => {

    try {

      await repository.migrateGraph();

    }catch(error) {
        console.error(error);
        res.send({ status: ok, statusCode: 500, body: { error } });
    }
};
