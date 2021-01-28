(define-resource publication-flow ()
  :class (s-prefix "pub:Publicatieaangelegenheid")
  :properties `((:publication-number  :string   ,(s-prefix "pub:publicatieNummer"))
                (:translate-before    :datetime ,(s-prefix "pub:uitersteVertaling")) ;; in de subcase ?
                (:publish-before      :datetime ,(s-prefix "pub:uiterstePublicatie")) ;; in de subcase ?
                (:published-at        :datetime ,(s-prefix "pub:publicatieOp")) ;; in de subcase ?                per drukproef/publicatie ??     per drukproef/publicatie ??
                (:numac-number        :string   ,(s-prefix "pub:numacNummer")) ;; object ? number ?  numac nummer per drukproef/publicatie ??
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
  :has-many `(  (mandatee             :via ,(s-prefix "ext:heeftBevoegde")
                                      :as "mandatees")
                (subcase              :via      ,(s-prefix "ext:doorloopt") ;; dossier:doorloopt kan niet, mu-cl-resources
                                      :as "subcases")
                (contact-person       :via      ,(s-prefix "pub:contactPersoon")
                                      :as "contact-persons"))
              ;; mandatees van subcase bij MR,  ongekende mandatee bij niet via MR
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/publicatie-aangelegenheden/")
  :features `(include-uri)
  :on-path "publication-flows")

(define-resource publication-status ()
  :class (s-prefix "pub:Publicatiestatus") ;; NOTE: as well as skos:Concept
  :properties `((:name        :string ,(s-prefix "skos:prefLabel"))
                (:priority    :number ,(s-prefix "ext:priority")))
  :has-many `((publication-flow    :via    ,(s-prefix "pub:publicatiestatus")
                              :inverse t
                              :as "publicaties"))
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


  ;; 
  ;; historiek ? momenteel weg gehaald, mogelijk later terug
  ;; aangevraagd door, instantie, heeft ook besluitdomeinen
  ;; persoon wie opmerking gemaakt heeft lijkt niet nodig
  ;; laatste wijziging door wie ?
  ;; create BY ?


  ;; done
  ;; status publicatie
  ;; uiterste vertaaldatum
  ;; uiterste datum publicatie
  ;; werknummer BS / numac
  ;; publicatie datum in BS
  ;; wijze van publicate (dct:type)
  ;; laatste wijziging
  ;; verstuurd naar BS ?
  ;; vertalingen krijgen fysiek een bestand, model maken paralell op serie? meerdere pieces die gelinkt zijn aan 1 iets, alle files zijn ofwel andere extensie of vertaald.
  ;; handtekeningen gebeuren op de PDF, vertalingen op de word
  ;; documenten (pdf en word aan elkaar kunnen hangen), opsplitsen ? 
  ;; contact persoon, dit is niet per se een bestaande user, gewoon text ?
  ;; opmerking 


  
