(define-resource agenda ()
  :class (s-prefix "besluitvorming:Agenda")
  :properties `((:issued :datetime ,(s-prefix "dct:issued"))
                (:isFinal :boolean ,(s-prefix "besluitvorming:finaleVersie"))
                (:name    :string  ,(s-prefix "ext:agendaNaam"))
                (:created :date    ,(s-prefix "ext:aangemaaktOp")))
  :has-one `(
            ;; (meeting :via ,(s-prefix "besluitvorming:behandelt") ;; NOTE: What is the URI of property 'behandelt'? Made up besluitvorming:behandelt
            ;;           :inverse t
            ;;           :as "meeting")
            (meeting :via ,(s-prefix "besluit:isAangemaaktVoor")
                     :as "created-for")
            (agenda :via ,(s-prefix "besluit:heeftAgenda")
                    :inverse t
                    :as "previous-version"))
  :has-many `((agendaitem :via ,(s-prefix "dct:hasPart")
                          :as "contains"))
  :resource-base (s-url "http://data.lblod.info/id/agendas/")
  :features '(include-uri)
  :on-path "agendas")

(define-resource agendaitem ()
  :class (s-prefix "besluit:Agendapunt")
  :properties `((:created :datetime ,(s-prefix "besluitvorming:aanmaakdatum")) ;; NOTE: What is the URI of property 'aanmaakdatum'? Made up besluitvorming:aanmaakdatum
                (:formally-ok :boolean ,(s-prefix "besluitvorming:formeelOK")) ;; NOTE: What is the URI of property 'formeelOK'? Made up besluitvorming:formeelOK
                (:retracted :boolean ,(s-prefix "besluitvorming:ingetrokken")) ;; NOTE: What is the URI of property 'ingetrokken'? Made up besluitvorming:ingetrokken
                (:priority :number ,(s-prefix "ext:priority"))
                (:for-press :number ,(s-prefix "ext:forPress")) 
                (:record :string ,(s-prefix "besluitvorming:notulen")) ;; NOTE: What is the URI of property 'notulen'? Made up besluitvorming:notulen
                (:title-press :string ,(s-prefix "besluitvorming:titelPersagenda"))) ;; NOTE: What is the URI of property 'titelPersagenda'? Made up besluitvorming:titelPersagenda
  :has-one `((postponed-to :via ,(s-prefix "ext:heeftVerdaagd") ;; instead of besluitvorming:verdaagd (mu-cl-resources relation type checking workaround)
                          :as "postponedTo")
             (agendaitem :via ,(s-prefix "besluit:aangebrachtNa")
                         :as "previousAgendaItem")
             (subcase :via ,(s-prefix "besluitvorming:isGeagendeerdVia")
                            :inverse t
                            :as "subcase")
             (decision :via ,(s-prefix "ext:agendapuntHeeftBesluit") ;; instead of prov:generated (mu-cl-resources relation type checking workaround)
                      :as "decision")
             (agenda :via ,(s-prefix "dct:hasPart")
                     :inverse t
                     :as "agenda"))
  :has-many `((mandatee :via ,(s-prefix "besluit:heeftAanwezige")
                     :inverse t
                     :as "attendees")
              ; (mededeling :via ,(s-prefix "?")
              ;                    :inverse t
              ;                    :as "mededelingen")
              (remark :via ,(s-prefix "besluitvorming:opmerking") ;; NOTE: opmerkingEN would be more suitable?
                      :as "remarks"))
  :resource-base (s-url "http://data.lblod.info/id/agendapunten/")
  :features '(include-uri)
  :on-path "agendaitems")

; (define-resource mededeling ()
;   :class (s-prefix "ext:Mededeling") ;; NOTE: 
;   :properties `((:? :? ,(s-prefix "?")))
;   :resource-base (s-url "http://data.vlaanderen.be/id/Mededeling/")
;   :features '(include-uri)
;   :on-path "mededelingen")


(define-resource postponed ()
  :class (s-prefix "besluitvorming:Verdaagd")
  :properties `((:postponed :boolean ,(s-prefix "besluitvorming:verdaagd")))
  :has-one `((meeting :via ,(s-prefix "besluitvorming:nieuweDatum") ;; instead of prov:generated (mu-cl-resources relation type checking workaround)
                      :inverse t
                      :as "subcase")
             (agendaitem :via ,(s-prefix "ext:heeftVerdaagd") ;; instead of besluitvorming:verdaagd (mu-cl-resources relation type checking workaround)
                         :inverse t
                         :as "agendaitems"))
  :resource-base (s-url "http://data.vlaanderen.be/id/Verdaagd/")
  :features '(include-uri)
  :on-path "postponeds")
  
(define-resource decision ()
  :class (s-prefix "besluit:Besluit") ;; NOTE: Took over all properties from document instead of subclassing (mu-cl-resources workaround)
  :properties `((:description :string ,(s-prefix "eli:description"))
                (:short-title :string ,(s-prefix "eli:title_short"))
                (:approved :boolean ,(s-prefix "besluitvorming:goedgekeurd")) ;; NOTE: What is the URI of property 'goedgekeurd'? Made up besluitvorming:goedgekeurd
                (:archived :boolean ,(s-prefix "besluitvorming:gearchiveerd")) ;; NOTE: Inherited from Document
                (:title :string-set ,(s-prefix "dct:title")) ;; NOTE: Inherited from Document
                (:number-vp :string ,(s-prefix "besluitvorming:stuknummerVP")) ;; NOTE: Inherited from Document ;; NOTE: What is the URI of property 'stuknummerVP'? Made up besluitvorming:stuknummerVP
                (:number-vr :string ,(s-prefix "besluitvorming:stuknummerVR"))) ;; NOTE: Inherited from Document
  :has-many `((mandatee :via ,(s-prefix "besluitvorming:neemtBesluit") ;; NOTE: What is the URI of property 'neemt' (Agent neemt besluit)? Guessed besluitvorming:neemtBesluit 
                          :inverse t
                          :as "mandatees")
              (remark :via ,(s-prefix "besluitvorming:opmerking") ;; NOTE: Inherited from Document
                                      :as "remarks")
              ; (documentversie :via ,(s-prefix "ext:documenttype")  ;; NOTE: Inherited from Document ;; NOTE: What is the URI of property 'heeftVersie'? Made up besluitvorming:heeftVersie
              ;           :as "heeft-versie")
              )
  :has-one `((subcase :via ,(s-prefix "ext:besluitHeeftProcedurestap") ;; instead of prov:generated (mu-cl-resources relation type checking workaround)
                            :inverse t
                            :as "subcase")
             (agendaitem :via ,(s-prefix "ext:agendapuntHeeftBesluit") ;; instead of prov:generated (mu-cl-resources relation type checking workaround)
                         :inverse t
                         :as "agendaitem")
             (publication :via ,(s-prefix "besluitvorming:isGerealiseerdDoor")
                          :as "publication")
             (newsletter-info :via ,(s-prefix "prov:generated") ;; instead of prov:generated (mu-cl-resources relation type checking workaround)
                              :as "newsletter-info")
             (document-type :via ,(s-prefix "ext:documentType") ;; NOTE: Inherited from Document
                            :as "type")
             (confidentiality :via ,(s-prefix "besluitvorming:vertrouwelijkheid") ;; NOTE: Inherited from Document
                              :as "confidentiality"))
  :resource-base (s-url "http://data.lblod.info/id/besluiten/")
  :features '(include-uri)
  :on-path "decisions")

;; TODO:karel translate
(define-resource bestuurseenheid ()
  :class (s-prefix "besluit:Bestuurseenheid")
  :properties `((:naam :string ,(s-prefix "skos:prefLabel")))
  :has-one `((werkingsgebied :via ,(s-prefix "besluit:werkingsgebied")
                             :as "werkingsgebied")
             (bestuurseenheid-classificatie-code :via ,(s-prefix "besluit:classificatie")
                                                 :as "classificatie")
             (vestiging :via ,(s-prefix "org:hasPrimarySite")
                        :as "primaire-site"))
  :has-many `((contact-punt :via ,(s-prefix "schema:contactPoint")
                            :as "contactinfo")
              (positie :via ,(s-prefix "org:hasPost")
                       :as "posities")
              (bestuursorgaan :via ,(s-prefix "besluit:bestuurt")
                              :inverse t
                              :as "bestuursorganen"))
  :resource-base (s-url "http://data.lblod.info/id/bestuurseenheden/")
  :features '(include-uri)
  :on-path "bestuurseenheden")

;; Unmodified from lblod/loket
(define-resource werkingsgebied ()
  :class (s-prefix "prov:Location")
  :properties `((:naam :string ,(s-prefix "rdfs:label"))
                (:niveau :string, (s-prefix "ext:werkingsgebiedNiveau")))
  :has-many `((bestuurseenheid :via ,(s-prefix "besluit:werkingsgebied")
                               :inverse t
                               :as "bestuurseenheid"))
  :resource-base (s-url "http://data.lblod.info/id/werkingsgebieden/")
  :features '(include-uri)
  :on-path "werkingsgebieden")

;; Unmodified from lblod/loket
(define-resource bestuurseenheid-classificatie-code ()
  :class (s-prefix "ext:BestuurseenheidClassificatieCode")
  :properties `((:label :string ,(s-prefix "skos:prefLabel"))
                (:scope-note :string ,(s-prefix "skos:scopeNote")))
  :resource-base (s-url "http://data.vlaanderen.be/id/concept/BestuurseenheidClassificatieCode/")
  :features '(include-uri)
  :on-path "bestuurseenheid-classificatie-codes")

;; Unmodified from lblod/loket
(define-resource bestuursorgaan ()
  :class (s-prefix "besluit:Bestuursorgaan")
  :properties `((:naam :string ,(s-prefix "skos:prefLabel"))
                (:binding-einde :date ,(s-prefix "mandaat:bindingEinde"))
                (:binding-start :date ,(s-prefix "mandaat:bindingStart")))
  :has-one `((bestuurseenheid :via ,(s-prefix "besluit:bestuurt")
                              :as "bestuurseenheid")
             (bestuursorgaan-classificatie-code :via ,(s-prefix "besluit:classificatie")
                                                :as "classificatie")
             (bestuursorgaan :via ,(s-prefix "mandaat:isTijdspecialisatieVan")
                             :as "is-tijdsspecialisatie-van"))
  :has-many `((bestuursorgaan :via ,(s-prefix "mandaat:isTijdspecialisatieVan")
                              :inverse t
                              :as "heeft-tijdsspecialisaties")
              (mandaat :via ,(s-prefix "org:hasPost")
                       :as "bevat"))
  :resource-base (s-url "http://data.lblod.info/id/bestuursorganen/")
  :features '(include-uri)
  :on-path "bestuursorganen")

;; Unmodified from lblod/loket
(define-resource bestuursorgaan-classificatie-code ()
  :class (s-prefix "ext:BestuursorgaanClassificatieCode")
  :properties `((:label :string ,(s-prefix "skos:prefLabel"))
                (:scope-note :string ,(s-prefix "skos:scopeNote")))
  :resource-base (s-url "http://data.vlaanderen.be/id/concept/BestuursorgaanClassificatieCode/")
  :features '(include-uri)
  :on-path "bestuursorgaan-classificatie-codes")

(define-resource meeting ()
  :class (s-prefix "besluit:Zitting")
  :properties `((:planned-start :datetime ,(s-prefix "besluit:geplandeStart")) 
                (:started-on :datetime ,(s-prefix "prov:startedAtTime")) ;; NOTE: Kept ':geplande-start' from besluit instead of ':start' from besluitvorming
                (:ended-on :datetime ,(s-prefix "prov:endedAtTime")) ;; NOTE: Kept ':geeindigd-op-tijdstip' from besluit instead of ':eind' from besluitvorming
                (:number :number ,(s-prefix "adms:identifier"))
                (:location :url ,(s-prefix "prov:atLocation"))) ;; NOTE: besluitvorming mentions (unspecified) type 'Locatie' TODO:karel
  :has-many `((agenda      :via ,(s-prefix "besluit:isAangemaaktVoor")
                           :inverse t
                           :as "agendas"))
  :has-one `((subcase :via ,(s-prefix "besluitvorming:isAangevraagdVoor")
                            :inverse t
                            :as "subcases")
             (agenda :via ,(s-prefix "besluitvorming:behandelt");; NOTE: What is the URI of property 'behandelt'? Made up besluitvorming:behandelt
                     :as "agenda"))

  :resource-base (s-url "http://data.lblod.info/id/zittingen/")
  :features '(include-uri)
  :on-path "meetings")
