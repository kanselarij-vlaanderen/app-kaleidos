/**
 * Dispatch the fetched information to a target graph.
 * @param { mu, muAuthSudo } lib - The provided libraries from the host service.
 * @param { termObjects } data - The fetched quad information, which objects of serialized Terms
 *          [ {
 *              graph: "<http://foo>",
 *              subject: "<http://bar>",
 *              predicate: "<http://baz>",
 *              object: "<http://boom>^^<http://datatype>"
 *            }
 *         ]
 * @return {void} Nothing
 */
async function dispatch(lib, data){
  const { mu, muAuthSudo } = lib;

  const triples = data.termObjects;

  console.log(`Found ${triples.length} to be processed`);
  console.log(`Showing only the first 10.`);
  const info = triples.slice(0,10).map(t => `triple: ${t.subject} ${t.predicate} ${t.object}`);
  info.forEach(s => console.log(s));
  console.log(`All triples were logged`);
}

module.exports = {
  dispatch
};
