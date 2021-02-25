(define-resource organization ()
  :class (s-prefix "org:Organization")
  :properties `((:name :string ,(s-prefix "skos:prefLabel"))
                (:created     :datetime       ,(s-prefix "dct:created"))
                (:modified    :datetime   ,(s-prefix "dct:modified"))
                ;;(:abbreviation :string ,(s-prefix "ISOcat:abbreviationFor"))
                (:identifier :string ,(s-prefix "org:identifier")))
  :has-one `((user                      :via      ,(s-prefix "ext:modifiedBy")
                                        :as "modified-by"))
  :has-many `((contact-person :via ,(s-prefix "org:memberOf")
                              :inverse t
                              :as "contact-persons"))
  :resource-base (s-url "https://data.vlaanderen.be/id/organisatie/")
  :features '(include-uri)
  :on-path "organizations"
)

