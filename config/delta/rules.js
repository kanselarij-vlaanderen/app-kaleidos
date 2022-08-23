export default [
  {
    match: {
      // form of element is {subject,predicate,object}
      subject: { // type: 'uri', value: 'ZZZZhttp://www.semanticdesktop.org/ontologies/2007/03/22/nmo#isPartOf' // ZZZZ in this example is just to ensure we don't match anything 8)
      }
    },
    callback: {
      url: 'http://search/update',
      method: 'POST'
    },
    options: {
      resourceFormat: "v0.0.1",
      gracePeriod: 1000,
      ignoreFromSelf: true
    }
  },
  {
    match: {
      // form of element is {subject,predicate,object}
      subject: { // type: 'uri', value: 'ZZZZhttp://www.semanticdesktop.org/ontologies/2007/03/22/nmo#isPartOf' // ZZZZ in this example is just to ensure we don't match anything 8)
      }
    },
    callback: {
      url: 'http://resource/.mu/delta',
      method: 'POST'
    },
    options: {
      resourceFormat: 'v0.0.1',
      gracePeriod: 250,
      ignoreFromSelf: true
    }
  },
  {
    match: {
      // could match a little closer if we were to watch all properties of agendas
      subject: {
      }
    },
    callback: {
      url: 'http://yggdrasil/delta',
      method: 'POST'
    },
    options: {
      resourceFormat: 'v0.0.1',
      gracePeriod: 10000,
      ignoreFromSelf: true
    }
  },
  {
    match: {
      predicate: {
        type: 'uri',
        value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'
      },
      object: {
        type: 'uri',
        value: 'http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject'
      }
    },
    callback: {
      url: 'http://file-bundling/delta',
      method: 'POST'
    },
    options: {
      resourceFormat: 'v0.0.1',
      gracePeriod: 250,
      ignoreFromSelf: false
    }
  },
  {
    match: {
      predicate: {
        type: 'uri',
        value: 'http://mu.semte.ch/vocabularies/ext/status'
      },
      object: {
        type: 'uri'
      }
    },
    callback: {
      url: 'http://file-bundling/delta',
      method: 'POST'
    },
    options: {
      resourceFormat: 'v0.0.1',
      gracePeriod: 250,
      ignoreFromSelf: true
    }
  },
  {
    match: {
      predicate: {
        type: 'uri',
        value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'
      },
      object: {
        type: 'uri',
        value: 'http://mu.semte.ch/vocabularies/ext/publicatie/PublicationMetricsExportJob'
      }
    },
    callback: {
      url: 'http://publication-report/delta',
      method: 'POST'
    },
    options: {
      resourceFormat: 'v0.0.1',
      gracePeriod: 500, // ensure PublicationMetricsExportJob relationships have been saved, since mu-cl-resources does that in different INSERT commands
      ignoreFromSelf: false
    }
  },
  {
    match: {
      predicate: {
        type: 'uri',
        value: 'http://data.vlaanderen.be/ns/besluitvorming#geagendeerdStuk'
      }
    },
    callback: {
      url: 'http://case-documents-sync/delta',
      method: 'POST'
    },
    options: {
      resourceFormat: 'v0.0.1',
      gracePeriod: 5000,
      ignoreFromSelf: true
    }
  },
  {
    match: {
      predicate: {
        type: 'uri',
        value: 'http://www.w3.org/ns/prov#generated'
      }
    },
    callback: {
      url: 'http://case-documents-sync/delta',
      method: 'POST'
    },
    options: {
      resourceFormat: 'v0.0.1',
      gracePeriod: 5000,
      ignoreFromSelf: true
    }
  },
  {
    match: {
      predicate: {
        type: 'uri',
        value: 'http://data.vlaanderen.be/ns/besluitvorming#beleidsveld'
      },
    },
    callback: {
      url: 'http://gov-field-sync/delta',
      method: 'POST'
    },
    options: {
      resourceFormat: 'v0.0.1',
      gracePeriod: 5000,
      ignoreFromSelf: true
    }
  },
  {
    match: {
      predicate: {
        type: 'uri',
        value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'
      },
      object: {
        type: 'uri',
        value: 'http://data.europa.eu/eli/ontology#LegalResource'
      }
    },
    callback: {
      url: 'http://staatsblad-uuid-generation/delta',
      method: 'POST'
    },
    options: {
      resourceFormat: 'v0.0.1',
      gracePeriod: 250,
      ignoreFromSelf: true
    }
  },
  {
    match: {
      predicate: {
        type: 'uri',
        value: 'http://data.europa.eu/eli/ontology#id_local'
      }
    },
    callback: {
      url: 'http://staatsblad-linking/delta',
      method: 'POST'
    },
    options: {
      resourceFormat: 'v0.0.1',
      gracePeriod: 15000,
      ignoreFromSelf: true
    }
  }
];
