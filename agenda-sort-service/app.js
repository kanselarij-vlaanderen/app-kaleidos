import mu from 'mu';
import { ok } from 'assert';

const app = mu.app;
const bodyParser = require('body-parser');
const repository = require('./repository');
const cors = require('cors');

app.use(bodyParser.json({ type: 'application/*+json' }));
app.use(cors());


app.post('/', async (req, res) => {

  let agendaId = req.query.agendaId;

  try {

      // DONE but double check
      const agendaItems = await repository.getMinistersWithBevoegdheidByAgendaId(agendaId);
      // TODO
      const prioritizedAgendaItems = await sortAgendaItemsByMandates(agendaItems);
      // TODO
      await repository.updateAgendaItemPriority(prioritizedAgendaItems);
      res.send({ status: ok, statusCode: 200, body: { items: prioritizedAgendaItems } });

  }catch(error) {
      console.error(error);
      res.send({ status: ok, statusCode: 500, body: { error } });
  }
});

// TODO Double check

const sortAgendaItemsByMandates = async (agendaItems) =>  {
    const prioritizedItems = [];

    for (let key in agendaItems){
        const item = agendaItems[key];
        let newPriority = await getHighestPriorityForAgendaItemMandates(item.mandates);
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

const getHighestPriorityForAgendaItemMandates = (mandates) => {
    let highestPriority = Number.MAX_SAFE_INTEGER;
    let priorities = mandates.map(item => item.priority);
    if (priorities){
      return Math.max(...priorities);
    }else {
        return highestPriority;
    }

};


