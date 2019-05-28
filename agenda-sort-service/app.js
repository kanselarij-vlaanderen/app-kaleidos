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
        const previousPrio = await repository.getLastPriorityOfAgendaitemInAgenda(agendaId);
        const prioritizedAgendaItems = await sortAgendaItemsByMandates(agendaItems, (previousPrio[0].maxPrio || 0));

        if (!queryOnly) {
            await repository.updateAgendaItemPriority(prioritizedAgendaItems);
        }

        res.send({ status: ok, statusCode: 200, body: { items: prioritizedAgendaItems } });

    } catch (error) {
        console.error(error);
        res.send({ status: ok, statusCode: 500, body: { error } });
    }
};

const sortAgendaItemsByMandates = async (agendaItems, previousPrio) => {
    agendaItems.sort((a, b) => {
        let priorityDiff = a.mandatePriority - b.mandatePriority;
        if (priorityDiff == 0) {
            return a.mandateeCount - b.mandateeCount;
        } else {
            return priorityDiff;
        }
    });
    for (let i = 0; i < agendaItems.length; i++) {
        agendaItems[i].priority = i + 1 + parseInt(previousPrio);
    }

    return agendaItems;
};


app.get('/new-filtering', async (req, res) => {

    const sessionId = req.query.sessionId;
    const agendaitems = await repository.getAllAgendaitemsOfTheSessionWithAgendaName(sessionId);
    const combinedAgendas = repository.reduceAgendaitemsToUniqueAgendas(agendaitems)
    const uniqueAgendaItems = [];
    console.log(agendaitems, combinedAgendas)
    Object.values(combinedAgendas).map((item => uniqueAgendaItems.push(...item.items)));

    console.log(uniqueAgendaItems)
    res.send(uniqueAgendaItems);
})
