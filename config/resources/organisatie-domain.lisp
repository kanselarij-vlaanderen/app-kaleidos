(define-resource organization ()
  :class (s-prefix "org:Organization")
  :properties `((:name :string ,(s-prefix "skos:prefLabel"))
                ;;(:abbreviation :string ,(s-prefix "ISOcat:abbreviationFor"))
                (:identifier :string ,(s-prefix "org:identifier")))
  :has-many `((contact-person :via ,(s-prefix "org:memberOf")
                              :inverse t
                              :as "contact-persons"))
  :resource-base (s-url "https://data.vlaanderen.be/id/organisatie/")
  :features '(include-uri)
  :on-path "organizations"
)

