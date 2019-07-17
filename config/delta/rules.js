export default [
  {
    match: {
      // form of element is {subject,predicate,object}
      subject: { // type: "uri", value: "ZZZZhttp://www.semanticdesktop.org/ontologies/2007/03/22/nmo#isPartOf" // ZZZZ in this example is just to ensure we don't match anything 8)
               }
    },
    callback: {
      url: "http://musearch/update",
      method: "POST"},
    options: {
      resourceFormat: "v0.0.0-genesis",
      gracePeriod: 2000,
      ignoreFromSelf: true
    }
  }
];
