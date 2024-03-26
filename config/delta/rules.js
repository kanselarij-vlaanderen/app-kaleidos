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
        value: 'https://data.vlaanderen.be/ns/besluitvorming#geagendeerdStuk'
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
        value: 'https://data.vlaanderen.be/ns/besluitvorming#beleidsveld'
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
  },
  /* Mirror sync producer */
  ...['http://mu.semte.ch/graphs/organizations/admin',
    'http://mu.semte.ch/graphs/organizations/intern-overheid',
    'http://mu.semte.ch/graphs/organizations/intern-regering',
    'http://mu.semte.ch/graphs/organizations/kanselarij',
    'http://mu.semte.ch/graphs/organizations/minister',
    'http://mu.semte.ch/graphs/public',
    'http://mu.semte.ch/graphs/system/email',
  ].map((graph) => {
    return {
      match: { graph: { value: graph } },
      callback: {
        url: 'http://delta-producer/delta',
        method: 'POST'
      },
      options: {
        resourceFormat: 'v0.0.1',
        gracePeriod: 1000
      }
    };
  }),
  /* Themis export: TTL to delta */
  {
    match: {
      predicate: {
        type: 'uri',
        value: 'http://www.w3.org/ns/adms#status'
      },
      object: {
        type: 'uri',
        value: 'http://redpencil.data.gift/ttl-to-delta-tasks/8C7E9155-B467-49A4-B047-7764FE5401F7'
      }
    },
    callback: {
      url: 'http://ttl-to-delta/delta', method: 'POST'
    },
    options: {
      resourceFormat: 'v0.0.1',
      gracePeriod: 1000,
      ignoreFromSelf: true
    }
  },
  /* Signflow activities lead to new status */
  {
    match: {
      predicate: {
        type: 'uri',
        value: 'http://mu.semte.ch/vocabularies/ext/handtekenen/markeringVindtPlaatsTijdens'
      }
    },
    callback: {
      url: 'http://signflow-status-sync/delta',
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
        value: 'http://mu.semte.ch/vocabularies/ext/handtekenen/voorbereidingVindtPlaatsTijdens'
      }
    },
    callback: {
      url: 'http://signflow-status-sync/delta',
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
        value: 'http://mu.semte.ch/vocabularies/ext/handtekenen/handtekeningVindtPlaatsTijdens'
      }
    },
    callback: {
      url: 'http://signflow-status-sync/delta',
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
        value: 'http://mu.semte.ch/vocabularies/ext/handtekenen/goedkeuringVindtPlaatsTijdens'
      }
    },
    callback: {
      url: 'http://signflow-status-sync/delta',
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
        value: 'http://mu.semte.ch/vocabularies/ext/handtekenen/weigeringVindtPlaatsTijdens'
      }
    },
    callback: {
      url: 'http://signflow-status-sync/delta',
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
        value: 'http://mu.semte.ch/vocabularies/ext/handtekenen/annulatieVindtPlaatsTijdens'
      }
    },
    callback: {
      url: 'http://signflow-status-sync/delta',
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
        value: 'http://mu.semte.ch/vocabularies/ext/handtekenen/afrondingVindtPlaatsTijdens'
      }
    },
    callback: {
      url: 'http://signflow-status-sync/delta',
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
      predicate: { // When the signing or approval activities start
        type: 'uri',
        value: 'https://data.vlaanderen.be/ns/dossier#Activiteit.startdatum'
      }
    },
    callback: {
      url: 'http://signflow-status-sync/delta',
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
      graph: {
        value: 'http://mu.semte.ch/graphs/organizations/kanselarij',
      },
      predicate: {
        type: 'uri',
        value: 'http://mu.semte.ch/vocabularies/ext/handtekenen/ongetekendStuk',
      }
    },
    callback: {
      url: 'http://pdf-flattener/delta',
      method: 'POST'
    },
    options: {
      resourceFormat: 'v0.0.1',
      gracePeriod: 500,
      ignoreFromSelf: true
    }
  },
  {
    match: {
      graph: {
        value: 'http://mu.semte.ch/graphs/organizations/kanselarij',
      },
      predicate: {
        type: 'uri',
        value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
      },
      object: {
        type: 'uri',
        value: 'https://data.vlaanderen.be/ns/dossier#Stuk',
      }
    },
    callback: {
      url: 'http://pdf-signature-remover/delta',
      method: 'POST'
    },
    options: {
      resourceFormat: 'v0.0.1',
      gracePeriod: 1000,
      ignoreFromSelf: true
    }
  }
];
