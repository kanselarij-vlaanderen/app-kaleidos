import mu from 'mu';
import { ok } from 'assert';

const app = mu.app;
const bodyParser = require('body-parser');
const repository = require('./repository');
const cors = require('cors');

app.use(bodyParser.json({ type: 'application/*+json' }));
app.use(cors());


app.get('/', (req, res) => {
    return getPostponedSubcases(req, res, true);
});

const getPostponedSubcases = async (req, res) => {

    try {

        const subcases = await repository.getPostponedSubcases();
        res.header('Content-Type' , 'application/vnd.api+json' );
        res.send({  data: subcases  });

    }catch(error) {
        console.error(error);
        res.send({ status: ok, statusCode: 500, body: { error } });
    }

};
