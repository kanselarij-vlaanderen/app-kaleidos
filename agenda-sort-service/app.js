import mu from 'mu';
import { ok } from 'assert';

const app = mu.app;
const bodyParser = require('body-parser');
const repository = require('./repository');
const cors = require('cors');

const priorities = [
    {
        priority: 1,
        responsibilities: ["Voorbeeld bevoegdheid" ]
    },
    {
        priority: 2,
        responsibilities: ["Cultuur", "Werk" ]
    },
    {
        priority: 3,
        responsibilities: ["Economie"]
    }
];

app.use(bodyParser.json({ type: 'application/*+json' }));
app.use(cors());


app.post('/', async (req, res) => {

  let agendaId = req.query.agendaId;

  try {

      const agendaItems = await repository.getMinistersWithBevoegdheidByAgendaId(agendaId);
      const prioritizedAgendaItems = await sortAgendaItemsByResponsibilities(agendaItems);
      await repository.updateAgendaItemPriority(prioritizedAgendaItems);

      res.send({ status: ok, statusCode: 200, body: { items: prioritizedAgendaItems } });

  }catch(error) {
        res.send({ status: ok, statusCode: 500, body: { error } });
  }
});

const sortAgendaItemsByResponsibilities = async (agendaItems) =>  {
    const prioritizedItems = [];

    for (let key in agendaItems){
        const item = agendaItems[key];
        let newPriority = await getHighestPriorityForAgendaItemConnections(item.connections);
        if (!newPriority || newPriority === Number.MAX_SAFE_INTEGER) newPriority = item.priority;
        item.newPriority = newPriority;
        prioritizedItems.push(item);
    }
    prioritizedItems.sort((a, b) => {
        return a.priority - b.priority;
    });
    for (let i = 0; i < prioritizedItems.length; i ++){
        prioritizedItems[i].priority = i + 1;
    }

    return prioritizedItems;
};

const getHighestPriorityForAgendaItemConnections = (connections) => {
    let highestPriority = Number.MAX_SAFE_INTEGER;
    let responsibilities = connections.map(item => item.responsibility);

    for (let i = 0; i < responsibilities.length; i++){
        const responsibility = responsibilities[i];
        const priority = getPriorityByResponsibility(responsibility);
        if (highestPriority > priority){
            highestPriority = priority;
        }
    }

    return highestPriority;
};

const getPriorityByResponsibility = (responsibility) => {
    let highestPriority = Number.MAX_SAFE_INTEGER;
    for (let i = 0; i < priorities.length; i++){
        const priority = priorities[i];
        if (priority.responsibilities.indexOf(responsibility) !== -1 && highestPriority > priority.priority) {
            highestPriority = priority.priority;
        }
    }
    return highestPriority;
};


