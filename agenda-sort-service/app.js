import mu from 'mu';
import { ok } from 'assert';

const app = mu.app;
const bodyParser = require('body-parser');
const repository = require('./repository');
const cors = require('cors');

app.use(bodyParser.json({ type: 'application/*+json' }));
app.use(cors());


app.post('/', (req, res) => {
  return handleSortRequest(req, res);
});

app.get('/', (req, res) => {
    return handleSortRequest(req, res, true);
  });
  
const handleSortRequest = async (req, res, queryOnly) => {
    let agendaId = req.query.agendaId;

    try {
  
        const agendaItems = await repository.getAgendaPriorities(agendaId);
        const prioritizedAgendaItems = await sortAgendaItemsByMandates(agendaItems);
        if(!queryOnly){
            await repository.updateAgendaItemPriority(prioritizedAgendaItems);
        }
        res.send({ status: ok, statusCode: 200, body: { items: prioritizedAgendaItems } });
  
    }catch(error) {
        console.error(error);
        res.send({ status: ok, statusCode: 500, body: { error } });
    }
};

const sortAgendaItemsByMandates = async (agendaItems) =>  {
    agendaItems.sort((a, b) => {
        let priorityDiff = a.mandatePriority - b.mandatePriority;
        if(priorityDiff == 0){
           return a.mandateeCount - b.mandateeCount;
        }else{
            return priorityDiff;
        }
    });
    for (let i = 0; i < agendaItems.length; i ++){
        agendaItems[i].priority = i + 1;
    }

    return agendaItems;
};
