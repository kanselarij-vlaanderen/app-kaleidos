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
    const currentAgendaID = req.query.selectedAgenda;
    const agendaitemsOfSelectedAgenda = await repository.getAllAgendaItemsFromAgenda(currentAgendaID);

    const agendaitems = await repository.getAllAgendaitemsOfTheSessionWithAgendaName(sessionId);
    const designAgendaItems = [];
    agendaitems.map((item) => {
        if (item.agendaName.toUpperCase() === "ONTWERPAGENDA") {
            designAgendaItems.push(item);
        }
    });

    console.log(designAgendaItems)
    setAllMappedPropertiesToTheAgendaItems(agendaitems, agendaitemsOfSelectedAgenda);
    const combinedAgendas = reduceAgendaitemsToUniqueAgendas(agendaitems);
    const combinedAgendasWithAgendaitems = getGroupedAgendaitems(combinedAgendas);

    res.send(combinedAgendasWithAgendaitems);
})

const reduceAgendaitemsPerTitle = (agendaitems) => {
    return agendaitems.reduce((agendaItems, agendaitem) => {
        agendaItems[agendaitem.groupTitle] = agendaItems[agendaitem.groupTitle] || { agendaitems: [] }
        agendaItems[agendaitem.groupTitle].agendaitems.push(agendaitem);
        return agendaItems;
    }, {});
}

const reduceMandateesToUniqueSubcases = (agendaitems) => {
    return agendaitems.reduce((agendaItems, agendaitem) => {
        agendaItems[agendaitem.subcaseId] = agendaItems[agendaitem.subcaseId] || { mandatees: [] }
        agendaItems[agendaitem.subcaseId].mandatees.push(agendaitem.title);
        return agendaItems;
    }, {});
}

const reduceAgendaitemsToUniqueAgendas = (agendaitems) => {
    const subcaseIdsParsed = [];
    return agendaitems.reduce((agendaItems, agendaitem) => {
        agendaItems[agendaitem.agendaName] = agendaItems[agendaitem.agendaName] || { items: [] }
        if (!subcaseIdsParsed.includes(agendaitem.subcase)) {
            delete agendaitem.mandatee;
            subcaseIdsParsed.push(agendaitem.subcase);
            agendaItems[agendaitem.agendaName].items.push(agendaitem);
        }

        return agendaItems;
    }, {});
}



const setAllMappedPropertiesToTheAgendaItems = (agendaitems, agendaitemsOfSelectedAgenda) => {
    const mandatees = reduceMandateesToUniqueSubcases(agendaitems);
    agendaitems.map((agendaitem) => {
        agendaitem['mandatees'] = [...new Set(mandatees[agendaitem.subcaseId].mandatees)];
        agendaitem['groupTitle'] = agendaitem['mandatees'].join(', ')
        const foundAgendaItem = agendaitemsOfSelectedAgenda.find((agendaitemToCheck) => agendaitemToCheck.subcaseId === agendaitem.subcaseId);
        if (foundAgendaItem) {
            agendaitem['agendaitem_id'] = foundAgendaItem.id;
        }
    });
}

const getGroupedAgendaitems = (combinedAgendas) => {
    return Object.entries(combinedAgendas).map((itemArray) => {
        if (itemArray[1].items.length > 0) {
            let obj = {
                agendaName: itemArray[0],
                groups: Object.entries(reduceAgendaitemsPerTitle(itemArray[1].items)).map((entry) => {
                    return {
                        title: entry[0],
                        agendaitems: entry[1].agendaitems
                    }
                })
            }

            return obj;
        }
    }).filter((item) => item);
}

/*
const filteredAgendaItems = await agendaitems.filter(agendaitem => {
			if (agendaitem && agendaitem.id) {
				const foundItem = agendaitemsFilteredPerAgenda.find(item => item.subcaseId === agendaitem.get('subcase.id'));
				if (foundItem) {
					if (!agendaitem.showAsRemark) {
						agendaitem.set('agendaName', foundItem.agendaName);
						agendaitem.set('foundPriority', agendaitem.priority);
						return agendaitem;
					}
				}
			}
        });
        **/