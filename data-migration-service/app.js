import mu from 'mu';

const app = mu.app;
const bodyParser = require('body-parser');
const cors = require('cors');
const cron = require('node-cron');

const fillInterneOverheid = require('./repository/fill-intern-overheid');
const fillInterneRegering = require('./repository/fill-intern-regering');

app.use(bodyParser.json({ type: 'application/*+json' }));
app.use(cors());

cron.schedule('* * * * *', async () => {
  fillInterneOverheid.fillUp();
  fillInterneRegering.fillUp();
  
});
