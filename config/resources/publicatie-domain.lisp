(define-resource config ()
  :class (s-prefix "pub:Config")
  :properties `((:key        :string ,(s-prefix "skos:prefLabel"))
                (:value      :string ,(s-prefix "pub:value")))
  :resource-base (s-url "http://themis.vlaanderen.be/id/concept/publicatie-config/")
  :features '(include-uri)
  :on-path "configs")

