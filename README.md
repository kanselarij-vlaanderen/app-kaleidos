# Kaleidos

[Kaleidos](https://overheid.vlaanderen.be/beleid-en-regelgeving/werking-en-besluitvorming-vlaamse-regering/kaleidos) is a software-platform used to support the decision-making-process of the Flemish government. The main software-stack is built upon [semantic.works](https://semantic.works/), a micro-service-based, [linked-data](https://en.wikipedia.org/wiki/Linked_data)-first architecture (previously known as [mu.semte.ch](https://mu.semte.ch/)). This repository contains the necessary configuration for each service that the stack is composed of. Please refer to the documentation of [mu-project](https://github.com/mu-semtech/mu-project/#mu-project) for more information on how to run and configure a *semantic.works*-based project.


## Development

A supplementary `docker-compose.development.yml`-file is provided in order to tweak the stack-setup for development purposes. Among other changes, this configuration will for instance prevent crashed services from restarting automatically, in order to catch errors quicker.

You can start the stack in development mode by running

```
docker-compose -f docker-compose.yml -f docker-compose.development.yml up
```

*Pro tip: The stack consists of some services such as `mu-search` that can potentially consume a lot of resources and often aren't required for basic development-tasks. Adding the following snippet to your `docker-compose.override.yml`-file under `services`, will disable the most resource-consuming services.*
```yml
  search:
    entrypoint: "echo 'service disabled'"
  tika:
    entrypoint: "echo 'service disabled'"
  elasticsearch:
    entrypoint: "echo 'service disabled'"
  yggdrasil:
    entrypoint: "echo 'service disabled'"
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

