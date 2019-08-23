export default [
  {
    match: {
      // form of element is {subject,predicate,object}
      subject: { // type: "uri", value: "ZZZZhttp://www.semanticdesktop.org/ontologies/2007/03/22/nmo#isPartOf" // ZZZZ in this example is just to ensure we don't match anything 8)
      }
    },
    callback: {
      url: "http://musearch/update",
      method: "POST"
    },
    options: {
      resourceFormat: "v0.0.0-genesis",
      gracePeriod: 2000,
      ignoreFromSelf: true
    }
  },
  {
    match: {
      // form of element is {subject,predicate,object}
      subject: { // type: "uri", value: "ZZZZhttp://www.semanticdesktop.org/ontologies/2007/03/22/nmo#isPartOf" // ZZZZ in this example is just to ensure we don't match anything 8)
      }
    },
    callback: {
      url: "http://resource/update",
      method: "POST"
    },
    options: {
      resourceFormat: "v0.0.1",
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
      url: "http://yggdrasil/delta",
      method: "POST"
    },
    options: {
      resourceFormat: "v0.0.1",
      gracePeriod: 10000,
      ignoreFromSelf: true
    }
  },



];
