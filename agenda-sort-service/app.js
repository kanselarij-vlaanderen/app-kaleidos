import mu from 'mu';
import { ok } from 'assert';

const app = mu.app;
const bodyParser = require('body-parser');

app.use(bodyParser.json({ type: 'application/*+json' }))

app.get('/', async (req, res) => {

  let agendaIds = req.query.agendaIds.split(',');
  agendaIds = agendaIds.map(item => parseInt(item));
  await sortByMinisterPriority(agendaIds);

  const hoedanigheden = await getHoedanigheden();
  let ids = await hoedanigheden.map(item => item.bevoegdheid);
  const bevoegdheden = await getBevoegdheden(ids);

  res.send({ status: ok, statusCode: 200, body: { agendaIds, hoedanigheden, bevoegdheden } });
});


const sortByMinisterPriority = async (agendaIds) => {
  return agendaIds.sort((a, b) => a - b);
};

const getHoedanigheden = async () => {

    const query = `
      PREFIX vo-org: <https://data.vlaanderen.be/ns/organisatie#>
      PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
      PREFIX vo-gen: <https://data.vlaanderen.be/ns/generiek#> 
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      
      SELECT ?hoedanigheid ?bevoegdheid ?label WHERE {
        GRAPH <http://mu.semte.ch/application> 
        {
        ?hoedanigheid a vo-org:Hoedanigheid ;
        mu:uuid ?uuid ;
        skos:prefLabel ?label ;
        vo-org:bevoegdheid ?bevoegdheid .
      }
    }`;

    let data = await mu.query(query);
    const vars = data.head.vars;

    return data.results.bindings.map(binding => {
        let obj = {};
        vars.forEach(varKey => {
            obj[varKey] = binding[varKey].value;
        });
        return obj;
    })

};

const getBevoegdheden = async (uris) => {

    const ids = uris.map(uri => uri.replace("http://localhost/vo/bevoegdheden/" , ""));

    const query = `
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
      PREFIX vo-gen: <https://data.vlaanderen.be/ns/generiek#> 
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      
      SELECT ?bevoegdheid ?uuid ?label WHERE {
        GRAPH <http://mu.semte.ch/application> 
        {
        ?bevoegdheid a ext:Bevoegdheid ;
        mu:uuid ?uuid ;
        skos:prefLabel ?label .
      }
    }`;

    // FILTER(?uuid = ${ids})

    let data = await mu.query(query);
    const vars = data.head.vars;

    return data.results.bindings.map(binding => {
        let obj = {};
        vars.forEach(varKey => {
            obj[varKey] = binding[varKey].value;
        });
        return obj;
    })

};

