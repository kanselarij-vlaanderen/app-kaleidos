import mu from 'mu';
import { ok } from 'assert';
import moment from 'moment';

const cors = require('cors');
const app = mu.app;
const bodyParser = require('body-parser');
const targetGraph = "http://mu.semte.ch/graphs/organizations/kanselarij";

app.use(cors());
app.use(bodyParser.json({ type: 'application/*+json' }));

app.get('/assignNewSessionNumbers', async function (req, res) {
  let sessions = await getAllSessions();

  for (let i = 0; i < sessions.length; i++) {
    sessions[i].number = i + 1;
  }

  const updatedDate = await updateSessionNumbers(sessions);
  res.send({ status: ok, statusCode: 200, body: { sessions: sessions, updateMessage: updatedDate } });
});

async function getAllSessions() {
  const firstDayOfTheYear = new Date(new Date().getFullYear(), 0, 1);

  const query = `
  PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
  PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
  
  SELECT ?session WHERE {
    GRAPH <${targetGraph}> 
    {
      ?session a besluit:Zitting ;
      mu:uuid ?uuid ;
      besluit:geplandeStart ?plannedstart .
      FILTER(str(?plannedstart) > "${firstDayOfTheYear.toISOString()}")
    }
  }
  ORDER BY ASC(?plannedstart)
  LIMIT 366`

  const data = await mu.query(query);
  const vars = data.head.vars;

  return data.results.bindings.map(binding => {
    let obj = {};
    vars.forEach(varKey => {
      obj[varKey] = binding[varKey].value;
    });
    return obj;
  })
}

function updateSessionNumbers(sessions) {
  let toDelete = [];
  let insertString = "";

  sessions.forEach(obj => {
    toDelete.push(`<${obj.session}>`);
    insertString = `${insertString}
    <${obj.session}> adms:identifier """${obj.number}"""^^xsd:decimal .
    `
  })

  const deleteString = toDelete.join();

  const query = `
  PREFIX adms: <http://www.w3.org/ns/adms#>
  
  DELETE WHERE { 
    GRAPH <${targetGraph}> { 
      ?target adms:identifier ?o .
      FILTER(?target IN (${deleteString}))
    } 
  };

  INSERT DATA { 
    GRAPH <${targetGraph}> { 
      ${insertString}
    } 
  }
  `
  return mu.update(query);
}

mu.app.use(mu.errorHandler);
