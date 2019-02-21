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

      const agendaItems = await repository.getMinistersWithBevoegdheidByAgendaId(agendaId);
      const prioritizedAgendaItems = await sortAgendaItemsByMandates(agendaItems);
      
      await repository.updateAgendaItemPriority(prioritizedAgendaItems);
      res.send({ status: ok, statusCode: 200, body: { items: prioritizedAgendaItems } });

  }catch(error) {
      console.error(error);
      res.send({ status: ok, statusCode: 500, body: { error } });
  }
});

const sortAgendaItemsByMandates = async (agendaItems) =>  {
    const prioritizedItems = [];

    for (let key in agendaItems){
        const item = agendaItems[key];
        let newPriority = await getLowestPriorityForAgendaItemMandates(item.mandates);
        if (!newPriority || newPriority === Number.MAX_SAFE_INTEGER){
            newPriority = item.priority;
        }
        item.newPriority = newPriority;
        prioritizedItems.push(item);
    }
    prioritizedItems.sort((a, b) => {
        let priorityDiff = a.newPriority - b.newPriority;
        if(priorityDiff == 0){
           return a.mandates.length - b.mandates.length;
        }else{
            return priorityDiff;
        }
    });
    for (let i = 0; i < prioritizedItems.length; i ++){
        prioritizedItems[i].newPriority = i + 1;
    }

    return prioritizedItems;
};

const getLowestPriorityForAgendaItemMandates = (mandates) => {
    let highestPriority = Number.MAX_SAFE_INTEGER;
    let priorities = mandates.map(item => item.priority);
    if (priorities){
      return Math.min(...priorities);
    }else {
        return highestPriority;
    }

};


