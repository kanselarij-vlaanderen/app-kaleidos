import mu from 'mu';
import { ok } from 'assert';

const app = mu.app;
const bodyParser = require('body-parser');
const repository = require('./repository');
const cors = require('cors');

const dotenv = require('dotenv');
dotenv.config();

const Mailchimp = require('mailchimp-api-v3');
const mailchimp = new Mailchimp(process.env.MAILCHIMP_API);
const moment = require('moment');
moment.locale("nl");

app.use(bodyParser.json({ type: 'application/*+json' }));
app.use(cors());

import { createNewsLetter } from './html-renderer/NewsLetter'
import { getNewsItem } from './html-renderer/NewsItem'

app.post('/mails', (req, res) => {
  return sendNewsletter(req, res);
});


app.get('/', (req, res) => {
  return getMostRecentNewsletter(req, res);
});


const getMostRecentNewsletter = async (req, res) => {

  try {

    const agendaId = req.query.agendaId;
    if (!agendaId){
      throw new Error("Request parameter agendaId can not be null");
    }

    let newsletter = await repository.getNewsletterInfo(agendaId);
    if (!newsletter){
      throw new Error("no newsletters present");
    }

    res.send({ newsletter })

  }catch(error) {
    console.error(error);
    res.send({ status: ok, statusCode: 500, body: { error } });
  }
};





const sendNewsletter = async (req, res) => {

    try {

      const agendaId = req.query.agendaId;
      if (!agendaId){
        throw new Error("Request parameter agendaId can not be null");
      }

      let newsletter = await repository.getNewsletterInfo(agendaId);
      if (!newsletter){
        throw new Error("no newsletters present");
      }

      const planned_start = moment(newsletter[0].planned_start).format("dddd DD-MM-YYYY");
      const news_items_HTML =  await newsletter.map(item => getNewsItem(item));
      let html = await createNewsLetter(news_items_HTML, planned_start);

      const template = {
        "name": `Nieuwsbrief ${planned_start}`,
        html
      };

      const created_template = await mailchimp.post({
        path : '/templates',
        body : template,
      });

      const { id } = created_template;
      const campaign = {
        "type": "regular",
        "recipients": {
          "list_id": "51ca8b6b84"
        },
        "settings": {
          "subject_line": `Nieuwsbrief ${planned_start}`,
          "preview_text": "",
          "title": `Nieuwsbrief ${planned_start}`,
          "from_name": "Tom Ploem",
          "reply_to": "info@liskdiscovery.com",
          "inline_css": true,
          "template_id": id
        }
      };
      await mailchimp.post({
        path : '/campaigns',
        body : campaign,
      });

      res.send({ campaign })

    }catch(error) {
        console.error(error);
        res.send({ status: ok, statusCode: 500, body: { error } });
    }
};



