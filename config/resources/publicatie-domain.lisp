(define-resource publication-flow ()
  :class (s-prefix "pub:Publicatieaangelegenheid")
  :properties `((:publication-number  :number   ,(s-prefix "pub:publicatieNummer"))
                (:publication-suffix  :string   ,(s-prefix "pub:publicatieNummerSuffix"))
                (:translate-before    :datetime ,(s-prefix "pub:uitersteVertaling")) ;; in de subcase ?
                (:publish-before      :datetime ,(s-prefix "pub:uiterstePublicatie")) ;; in de subcase ?
                (:publish-date-requested    :datetime ,(s-prefix "pub:gevraagdePublicatie"))
                (:published-at        :datetime ,(s-prefix "pub:publicatieOp")) ;; in de subcase ?                per drukproef/publicatie ??     per drukproef/publicatie ??
                (:remark              :string   ,(s-prefix "pub:publicatieOpmerking")) ;; check of dit nodig is
                (:priority            :number   ,(s-prefix "pub:prioriteit"))
                (:created             :datetime ,(s-prefix "dct:created"))
                (:modified            :datetime ,(s-prefix "dct:modified")))
  :has-one `((case                    :via      ,(s-prefix "dossier:behandelt")
                                      :as "case")
             (publication-status      :via      ,(s-prefix "pub:publicatiestatus")
                                      :as "status")
             (publication-type        :via      ,(s-prefix "dct:type")
                                      :as "type"))
  :has-many `((mandatee               :via      ,(s-prefix "ext:heeftBevoegdeVoorPublicatie")
                                      :as "mandatees")
              (numac-number           :via      ,(s-prefix "pub:numacNummer")
                                      :as "numac-numbers")
              (subcase                :via      ,(s-prefix "ext:doorloopt") ;; dossier:doorloopt kan niet, mu-cl-resources
                                      :as "subcases")
              (contact-person         :via      ,(s-prefix "pub:contactPersoon")
                                      :as "contact-persons"))
              ;; mandatees van subcase bij MR,  ongekende mandatee bij niet via MR
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/publicatie-aangelegenheden/")
  :features `(include-uri)
  :on-path "publication-flows")

(define-resource numac-number ()
  :class (s-prefix "pub:NumacNummer")
  :properties `((:name                :string ,(s-prefix "skos:prefLabel")))
  :has-one `((publication-flow        :via      ,(s-prefix "pub:numacNummer")
                                      :inverse t
                                      :as "publication-flow"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/concept/publicatie-numac-nummers/")
  :features '(include-uri)
  :on-path "numac-numbers")

(define-resource publication-status ()
  :class (s-prefix "pub:Publicatiestatus") ;; NOTE: as well as skos:Concept
  :properties `((:name        :string ,(s-prefix "skos:prefLabel"))
                (:priority    :number ,(s-prefix "ext:priority")))
  :has-many `((publication-flow    :via    ,(s-prefix "pub:publicatiestatus")
                              :inverse t
                              :as "publications"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/concept/publicatie-statussen/")
  :features '(include-uri)
  :on-path "publication-statuses")

(define-resource language ()
  :class (s-prefix "euvoc:Language") ;; range of dct:language is a dct:LinguisticSystem. Also see https://github.com/SEMICeu/DCAT-AP/issues/55
  :properties `((:name                 :string ,(s-prefix "skos:prefLabel"))
                (:priority             :number ,(s-prefix "ext:priority")))
  :features '(include-uri)
  :on-path "languages")

(define-resource publication-type ()
  :class (s-prefix "ext:PublicatieType") ;; NOTE: as well as skos:Concept
  :properties `((:label           :string ,(s-prefix "skos:prefLabel"))
                (:scope-note      :string ,(s-prefix "skos:scopeNote"))
                (:alt-label       :string ,(s-prefix "skos:altLabel"))
                (:priority        :number ,(s-prefix "ext:priority")))
  :has-many `((publication-flow   :via ,(s-prefix "dct:type")
                                  :inverse t
                                  :as "publication-flows"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/concept/publicatie-types/")
  :features '(include-uri)
  :on-path "publication-types")

  
