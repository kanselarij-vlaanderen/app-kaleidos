(in-package :mu-cl-resources)

;;;;
;; NOTE
;; docker-compose stop; docker-compose rm; docker-compose up
;; after altering this file.


;;;;
;; Describe the prefixes which you'll use in the domain file here.
;; This is a short-form which allows you to write, for example,
;; (s-url "http://purl.org/dc/terms/title")
;; as (s-prefix "dct:title")
(add-prefix "vobesluit" "https://data.vlaanderen.be/ns/besluitvorming#")
(add-prefix "voorg" "https://data.vlaanderen.be/ns/organisatie#")
(add-prefix "vogen" "https://data.vlaanderen.be/ns/generiek#")
(add-prefix "dbpedia" "http://dbpedia.org/ontology/")
(add-prefix "dcat" "http://www.w3.org/ns/dcat#")
(add-prefix "dct" "http://purl.org/dc/terms/")
(add-prefix "skos" "http://www.w3.org/2004/02/skos/core#")
(add-prefix "provo" "http://www.w3.org/ns/prov#")
(add-prefix "nfo" "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#")
(add-prefix "nie" "http://www.semanticdesktop.org/ontologies/2007/01/19/nie#")
(add-prefix "dbpedia" "http://dbpedia.org/ontology/")
(add-prefix "foaf" "http://xmlns.com/foaf/0.1/")

;;;;;
;; You can use the muext: prefix when you're still searching for
;; the right predicates during development.  This should *not* be
;; used to publish any data under.  It's merely a prefix of which
;; the mu.semte.ch organisation indicates that it will not be used
;; by them and that it shouldn't be used for permanent URIs.


(add-prefix "ext" "http://mu.semte.ch/vocabularies/ext/")
(add-prefix "oc" "http://mu.semte.ch/vocabularies/ext/oc/")
(add-prefix "tmp" "http://mu.semte.ch/vocabularies/tmp/")

(add-prefix "besluit" "http://data.vlaanderen.be/ns/besluit#")
(add-prefix "besluitvorming" "http://data.vlaanderen.be/ns/besluitvorming#")
(add-prefix "generiek" "http://data.vlaanderen.be/ns/generiek#")
(add-prefix "mandaat" "http://data.vlaanderen.be/ns/mandaat#")
(add-prefix "persoon" "http://data.vlaanderen.be/ns/persoon#")

(add-prefix "adms" "http://www.w3.org/ns/adms#")
(add-prefix "bbcdr" "http://mu.semte.ch/vocabularies/ext/bbcdr/")
(add-prefix "cpsv" "http://purl.org/vocab/cpsv#")
(add-prefix "dbpedia" "http://dbpedia.org/ontology/")
(add-prefix "eli" "http://data.europa.eu/eli/ontology#")
(add-prefix "dct" "http://purl.org/dc/terms/")
(add-prefix "dul" "http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#")
(add-prefix "export" "http://mu.semte.ch/vocabularies/ext/export/")
(add-prefix "foaf" "http://xmlns.com/foaf/0.1/")
(add-prefix "m8g" "http://data.europa.eu/m8g/")
(add-prefix "nao" "http://www.semanticdesktop.org/ontologies/2007/08/15/nao#")
(add-prefix "nfo" "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#")
(add-prefix "nie" "http://www.semanticdesktop.org/ontologies/2007/01/19/nie#")
(add-prefix "nmo" "http://www.semanticdesktop.org/ontologies/2007/03/22/nmo#")
(add-prefix "org" "http://www.w3.org/ns/org#")
(add-prefix "pav" "http://purl.org/pav/")
(add-prefix "person" "http://www.w3.org/ns/person#")
(add-prefix "prov" "http://www.w3.org/ns/prov#")
(add-prefix "regorg" "https://www.w3.org/ns/regorg#")
(add-prefix "rdfs" "https://www.w3.org/2000/01/rdf-schema#")
(add-prefix "schema" "http://schema.org/")
(add-prefix "skos" "http://www.w3.org/2004/02/skos/core#")
(add-prefix "toezicht" "http://mu.semte.ch/vocabularies/ext/supervision/")
(add-prefix "toezichtReport" "http://mu.semte.ch/vocabularies/ext/supervision/reporting/")
(add-prefix "validation" "http://mu.semte.ch/vocabularies/validation/")

(add-prefix "email" "http://mu.semte.ch/vocabularies/ext/email/")
(add-prefix "kans" "http://kanselarij.vo.data.gift/core/")


;;;;;
;; You can use the muext: prefix when you're still searching for
;; the right predicates during development.  This should *not* be
;; used to publish any data under.  It's merely a prefix of which
;; the mu.semte.ch organisation indicates that it will not be used
;; by them and that it shouldn't be used for permanent URIs.

(add-prefix "ext" "http://mu.semte.ch/vocabularies/ext/")
(add-prefix "tmp" "http://mu.semte.ch/vocabularies/tmp/")
