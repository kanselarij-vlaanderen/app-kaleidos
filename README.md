# Kaleidos

[Kaleidos](https://overheid.vlaanderen.be/beleid-en-regelgeving/werking-en-besluitvorming-vlaamse-regering/kaleidos) is a software-platform used to support the decision-making-process of the Flemish government. The main software-stack is built upon [semantic.works](https://semantic.works/), a micro-service-based, [linked-data](https://en.wikipedia.org/wiki/Linked_data)-first architecture (previously known as [mu.semte.ch](https://mu.semte.ch/)). This repository contains the necessary configuration for each service that the stack is composed of. Please refer to the documentation of [mu-project](https://github.com/mu-semtech/mu-project/#mu-project) for more information on how to run and configure a *semantic.works*-based project.


## Development

A supplementary `docker-compose.development.yml`-file is provided in order to tweak the stack-setup for development purposes. Among other changes, this configuration will for instance prevent crashed services from restarting automatically, in order to catch errors quicker. It also disables certain services that generally aren't required for development purposes. This is done by replacing 

You can start the stack in development mode by running

```
docker-compose -f docker-compose.yml -f docker-compose.development.yml up
```

*Pro tip: The stack consists of some services such as `mu-search` that can potentially consume a lot of resources and often aren't required for basic development-tasks. Adding the following snippet to your `docker-compose.override.yml`-file under `services` will replace the most resource-consuming services with "sink" services that accept all requests but do nothing. This saves on your resources and prevents certain errors from occurring due to the services being down.*
```yml
  search:
    image: lblod/sink-service:1.0.0
  tika:
    image: lblod/sink-service:1.0.0
  elasticsearch:
    image: lblod/sink-service:1.0.0
  yggdrasil:
    image: lblod/sink-service:1.0.0
```

If you need to re-enable a service that is disabled in `docker-compose.development.yml` you can do so by adding the the service to your `docker-compose.override.yml` with the image that's defined in `docker-compose.yml`.

``` yml
# e.g. delta-consumer is disabled but you want to re-enable it
  delta-consumer:
    image: kanselarij/delta-consumer:0.0.1
```

## Deployment

### Mirror

When deploying the app and if you want to enable mirroring, such that all changes are synced to a (fail-over) server, you will need to update some of the environment variables in your `docker-compose.override.yml` file.
Make sure that your mirrored server contains a data sync of your main server, as the mirroring does not do an initial sync of the whole database.

``` yml
# !! MAIN SERVER !!
  delta-producer:
    environment:
      KEY: "put an actual random key here" # We need this so the two server can communicate
  delta-consumer:
    image: lblod/sink-service:1.0.0 # The consumer isn't needed here, so we might as well disable it

# !! MIRROR SERVER !!
  delta-consumer:
    environment:
      DCR_SECRET_KEY: "put the same random key you put in the delta-producer here"
      DCR_SYNC_BASE_URL: "http://url-to-the-main-server.com"
      DCR_START_FROM_DELTA_TIMESTAMP: "2023-01-01T00:00:00Z" # This timestamp should be recently in the past as of deploy time, so that you only try to ingest recent changes
  delta-producer:
    image: lblod/sink-service:1.0.0
  fileshare:
    image: lblod/sink-service:1.0.0
```

## Data

Most of Kaleidos' data is structured according the vocabularies & application-profiles standardised through [OSLO](https://data.vlaanderen.be/). The ones used in Kaleidos include:

- [Besluitvorming](https://data.vlaanderen.be/doc/applicatieprofiel/besluitvorming/)
- [Besluit Publicatie](https://data.vlaanderen.be/doc/applicatieprofiel/besluit-publicatie/)
- [Dossier](https://data.vlaanderen.be/doc/applicatieprofiel/dossier/)
- [Mandatendatabank](https://data.vlaanderen.be/doc/applicatieprofiel/mandatendatabank/)

### Open-data publication to Themis

Part of the data produced within Kaleidos is published as open-data through the "Themis"-portal. Please consult https://themis.vlaanderen.be/ for more information.

The sync to Themis works with a pull-mechanism. Kaleidos generates and provides publications, which are polled and fetched at regular intervals by the Themis stack.

The services used to generate data exports for Themis:
- an [export service](https://github.com/kanselarij-vlaanderen/themis-export-service) responsible for collecting data from Kaleidos and generating a TTL data dump
- a [TTL to delta conversion service](https://github.com/redpencilio/ttl-to-delta-service) to convert the TTL data dump to the delta format
- a [producer service](https://github.com/kanselarij-vlaanderen/themis-publication-producer) providing an endpoint to fetch publications for interested consumers
- a [public file service](https://github.com/kanselarij-vlaanderen/public-file-service) providing an endpoint for interested consumers to fetch public Kaleidos documents

The main component of the [Themis stack](https://github.com/kanselarij-vlaanderen/app-themis) is the [Themis publication consumer service](https://github.com/kanselarij-vlaanderen/themis-publication-consumer) responsible for polling and fetching of the publications and accompanying documents.

