(define-resource organization ()
  :class (s-prefix "org:Organization")
  :properties `((:name :string ,(s-prefix "skos:prefLabel"))
                (:identifier :string ,(s-prefix "org:identifier")))
  :has-many `((user :via ,(s-prefix "org:memberOf")
                             :inverse t
                             :as "members"))
  :resource-base (s-url "https://data.vlaanderen.be/id/organisatie/")
  :features '(include-uri)
  :on-path "organizations"
)

