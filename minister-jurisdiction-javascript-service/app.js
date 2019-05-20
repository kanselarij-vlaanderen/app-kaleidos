import mu from 'mu';
import { ok } from 'assert';

const app = mu.app;
const bodyParser = require('body-parser');
const repository = require('./repository');
const cors = require('cors');

app.use(bodyParser.json({ type: 'application/*+json' }));
app.use(cors());

app.post('/mails', (req, res) => {
	return sendNewsletter(req, res);
});

app.get('/', (req, res) => {
	return getMostRecentNewsletter(req, res);
});

const getMostRecentNewsletter = async (req, res) => {

	try {

	}catch(error) {
		console.error(error);
		res.send({ status: ok, statusCode: 500, body: { error } });
	}
};

const sendNewsletter = async (req, res) => {

	try {


	}catch(error) {
		console.error(error);
		res.send({ status: ok, statusCode: 500, body: { error } });
	}
};



