(define-resource config ()
  :class (s-prefix "pub:Config")
  :properties `((:key        :string ,(s-prefix "skos:prefLabel"))
                (:value      :string ,(s-prefix "pub:value")))
  :resource-base (s-url "http://themis.vlaanderen.be/id/concept/publicatie-config/")
  :features '(include-uri)
  :on-path "configs")

(define-resource numac-number ()
  :class (s-prefix "pub:NumacNummer")
  :properties `((:name                :string ,(s-prefix "skos:prefLabel")))
  :has-one `((publication-flow        :via      ,(s-prefix "pub:numacNummer")
                                      :inverse t
                                      :as "publication-flow"))
  :resource-base (s-url "http://themis.vlaanderen.be/id/numac-nummer/")
  :features '(include-uri)
  :on-path "numac-numbers")

(define-resource publication-type ()
  :class (s-prefix "ext:PublicatieType") ;; NOTE: as well as skos:Concept  ;; TODO shouldn't this be pub: ?
  :properties `((:label           :string ,(s-prefix "skos:prefLabel"))
                (:scope-note      :string ,(s-prefix "skos:scopeNote"))
                (:alt-label       :string ,(s-prefix "skos:altLabel"))
                (:priority        :number ,(s-prefix "ext:priority")))
  :has-many `((publication-flow   :via ,(s-prefix "dct:type")
                                  :inverse t
                                  :as "publication-flows"))
  :resource-base (s-url "http://themis.vlaanderen.be/id/concept/publicatie-type/")
  :features '(include-uri)
  :on-path "publication-types")



