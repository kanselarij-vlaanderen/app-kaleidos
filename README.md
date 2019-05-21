# mu-project

Bootstrap a mu.semte.ch microservices environment in three easy steps.

## How to

Setting up your environment is done in three easy steps:  first you configure the running microservices and their names in `docker-compose.yml`, then you configure how requests are dispatched in `config/dispatcher.ex`, and lastly you start everything.

### Hooking things up with docker-compose

Alter the `docker-compose.yml` file so it contains all microservices you need.  The example content should be clear, but you can find more information in the [Docker Compose documentation](https://docs.docker.com/compose/).  Don't remove the `identifier` and `db` container, they are respectively the entry-point and the database of your application.  Don't forget to link the necessary microservices to the dispatcher and the database to the microservices.

### Configure the dispatcher

Next, alter the file `config/dispatcher.ex` based on the example that is there by default.  Dispatch requests to the necessary microservices based on the names you used for the microservice.

### Boot up the system

# This project has a different approach to start the compose.

We use 3 docker-compose.yml files to divide our environments.
> docker-compose -f docker-compose.local.yml 

Will connect your docker-compose command to the docker-compose.local.yml file. Once this is done you will be able to build and run the docker-compose locally by executing
> docker-compose up -d


You can shut down using `docker-compose stop` and remove everything using `docker-compose rm`, to force your compose to stop, we use  `docker-compose kill`.

# Data needed to run the frontend.

All ttl files needed are located in the `/data` folder.

To be able to use the frontend you should upload all files listed below in the `http://mu.semte.ch/graphs/public` and to `http://mu.semte.ch/graphs/organizations/kanselarij`

- `alert-types.ttl`
- `confidentiality.ttl`
- `document-types.ttl`
- `dossier-type-codes.ttl`
- `government-domains.ttl`
- `government-fields.ttl`
- `ise-codes.ttl`
- `ministers.ttl`
- `mock-roles.ttl`
- `policy-levels.ttl`
- `procedurestap-fase-codes.ttl`
- `procedurestap-types.ttl`
- `themes.ttl`
- `submitters.ttl`

After doing this, you should be able to run the frontend without any troubles.
