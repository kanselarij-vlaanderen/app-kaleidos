import mu from 'mu';
import { ok } from 'assert';

const dotenv = require('dotenv');
dotenv.config();

const app = mu.app;
const bodyParser = require('body-parser');
const repository = require('./repository');
const cors = require('cors');

const Mailchimp = require('mailchimp-api-v3');
const mailchimp = new Mailchimp(process.env.MAILCHIMP_API);
const moment = require('moment');
moment.locale("nl");

app.use(bodyParser.json({ type: 'application/*+json' }));
app.use(cors());

import nieuwsbrief from './nieuwsbrief'


app.get('/', (req, res) => {
  return setNewsletter(req, res);
});

app.post('/', (req, res) => {
  return setNewsletter(req, res);
});


const setNewsletter = async (req, res) => {

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
      let html = await nieuwsbrief(news_items_HTML, planned_start);

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


const getNewsItem = ({ title, subtitle, text }) => {
  return `
    <table mc:repeatable="content" mc:variant="Tekstblok met introtekst" width="100%" cellpadding="0" cellspacing="0" border="0">
      <tr>
        <td height="30" style="height:30px;line-height:0;">

         </td>
      </tr>
      <tr>
        <td style="padding:5px 0 15px 0;">
          <font style="color:#333332;font-family:Calibri, Arial, sans-serif;font-size:26px;font-weight:600;line-height:26px;">${title}</font>
          <p class="intro-text" style="color:#666666;font-family:Calibri, Arial, sans-serif;font-size:15px;line-height:20px;margin-top:5px;margin-bottom:0;">
            ${subtitle}
          </p>
        </td>
      </tr>
      <tr>
        <td style="color:#666666;font-family:Calibri, Arial, sans-serif;font-size:17px;line-height:26px;">
          <p>${text}</p>
        </td>
      </tr>
    </table>
    
  `
};
