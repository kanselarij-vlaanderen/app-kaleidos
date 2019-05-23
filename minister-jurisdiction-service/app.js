import mu from 'mu';
import { ok } from 'assert';

const app = mu.app;
const bodyParser = require('body-parser');
const repository = require('./repository');
const cors = require('cors');

app.use(bodyParser.json({ type: 'application/*+json' }));
app.use(cors());


app.get('/domains/ministers', (req, res) => {
	return getMinistersForDomain(req, res);
});

app.get('/domain/mandatee', (req, res) => {
	return getMandateeForDomain(req, res);
});

app.get('/mandatee/domains', (req, res) => {
	return getDomainsForMandatee(req, res);
});

app.post('/domain/transfer', (req, res) => {
	return setMandateeOnDomain(req, res);
});

app.post('/transfer/procedures', (req, res) => {
	return addMandateeToSubCase(req, res);
});


const getMinistersForDomain = async (req, res) => {
	try {
		return await repository.getMinistersForDomain();
	}catch(error) {
		console.error(error);
		res.send({ status: ok, statusCode: 500, body: { error } });
	}
};

const getMandateeForDomain = async (req, res) => {
	try {
		let domain = req.query.domain;
		return await repository.getMandateeForDomain(domain);

	}catch(error) {
		console.error(error);
		res.send({ status: ok, statusCode: 500, body: { error } });
	}
};

const getDomainsForMandatee = async (req, res) => {
	try {
		let mandatee = req.query.mandatee;
		return await repository.getDomainsForMandatee(mandatee);
	}catch(error) {
		console.error(error);
		res.send({ status: ok, statusCode: 500, body: { error } });
	}
};

const setMandateeOnDomain = async (req, res) => {

	try {
		let { receiving_mandatee, transferred_domain } = req.body;
		return await repository.setMandateeOnDomain(receiving_mandatee, transferred_domain);
	}catch(error) {
		console.error(error);
		res.send({ status: ok, statusCode: 500, body: { error } });
	}
};

const addMandateeToSubCase = async (req, res) => {
	try {

		let new_mandatee = req.body.new_mandatee;
		let old_mandatee = req.body.old_mandatee;

		const open_subcases = await repository.getUniqueSubCaseWhereOpenByMandatee(old_mandatee);
		console.log(open_subcases);
		await repository.setMandateeOnSubCase(open_subcases, new_mandatee, old_mandatee);
		res.send({ status: ok });
	}catch(error) {
		console.error(error);
		res.send({ status: ok, statusCode: 500, body: { error } });
	}
};
