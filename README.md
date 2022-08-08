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

Part of the data produced within Kaleidos will soon be published as open-data through the "Themis"-portal. Please consult https://themis.vlaanderen.be/ for more information.

## URI formatting guidelines

When we create new resources, either by adding data in the application, or by adding data through `config/migrations/`, a URI is needed to uniquely identify each resource. For types and properties, we can use the URIs as defined in the OSLO data model, but for resources, we need to define our own. For future reference, we'll set up some guidelines here to help keep the formatting of those URIs consistent moving forward.

The [Flemish URI Standard for Data](https://assets.vlaanderen.be/image/upload/v1637336713/Vlaamse_URI-standaard_voor_data_ifmzka.pdf) specifies the following structure for URIs: `http(s)://{domain}/{type}/{concept}(/{reference})`.

For resources in Kaleidos, this means the `domain` and `type` parts of a resource URI will be `themis.vlaanderen.be` and `id`, in accordance with the Flemish URI Standard.

The `concept` part needs to be a semantically understandable name to idenitfy the **category** of the resource, e.g., 'mandatee', 'persoon' or 'concept/toegangsniveau'. Note that the `concept` part can be nested, depending on the scenario.

Finally, for the `reference` part, the standard does not specify any obligatory structure, and instead leaves this up to the organisation. For Kaleidos, we'll maintain the rule: use a **uuid** for each resource, e.g., `http://themis.vlaanderen.be/id/mandatee/145bafde-10ea-4a73-8fe9-042cdea3ccbe`. 

*Remark: While this is intuitive for resources referencing real-world things such as persons or objects, we will also apply this to semantic concepts, such as concept types or access levels, e.g., `http://themis.vlaanderen.be/id/concept/toegangsniveau/c3de9c70-391e-4031-a85e-4b03433d6266`. While it might be tempting to give the latter type a more human-readable, semantically appealing URI, such as `http://themis.vlaanderen.be/id/concept/toegangsniveau/publiek`, this does open the door for naming conflicts, label mismatches, semantic misunderstandings, etc. Therefore, the rule is and remains: stick to uuids.*
