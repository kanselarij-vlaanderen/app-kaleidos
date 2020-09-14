(define-resource agenda ()
  :class (s-prefix "besluitvorming:Agenda")
  :properties `((:issued      :datetime   ,(s-prefix "dct:issued"))
                (:title        :string     ,(s-prefix "dct:title"))
                (:serialnumber :string    ,(s-prefix "besluitvorming:volgnummer"))
                (:created     :datetime       ,(s-prefix "dct:created"))
                ; (:agendatype  :url        ,(s-prefix "dct:type")) // Currently not implemented ( https://test.data.vlaanderen.be/doc/applicatieprofiel/besluitvorming/#Agenda )
                (:modified    :datetime   ,(s-prefix "dct:modified")))
  :has-one `((meeting         :via        ,(s-prefix "besluitvorming:isAgendaVoor")
                              :as "created-for")
             (agendastatus    :via        ,(s-prefix "besluitvorming:agendaStatus")
                              :as "status")
             (access-level    :via ,(s-prefix "besluitvorming:vertrouwelijkheidsniveau")
                              :as "access-level")
             (agenda          :via        ,(s-prefix "prov:wasRevisionOf")
                              :as "previous-version")
             (agenda           :via        ,(s-prefix "prov:wasRevisionOf")
                               :inverse t
                               :as "next-version"))
  :has-many `((agendaitem     :via        ,(s-prefix "dct:hasPart")
                              :as "agendaitems")
              (piece       :via        ,(s-prefix "besluitvorming:heeftBijlage")
                              :as "attachments"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/agendas/")
  :features '(include-uri)
  :on-path "agendas")

(define-resource agendastatus ()
  :class (s-prefix "kans:AgendaStatus")
  :properties `((:label       :string ,(s-prefix "skos:prefLabel"))
                  (:scope-note  :string ,(s-prefix "skos:scopeNote"))
                  (:alt-label   :string ,(s-prefix "skos:altLabel")))
  :has-many `((agenda     :via        ,(s-prefix "besluitvorming:agendaStatus")
                          :inverse t
                          :as "agendas"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/agendastatus/")
  :features '(include-uri)
  :on-path "agendastatuses")

(define-resource agendaitem ()
  :class (s-prefix "besluit:Agendapunt")
  :properties `((:created             :datetime ,(s-prefix "besluitvorming:aanmaakdatum")) ;; NOTE: What is the URI of property 'aanmaakdatum'? Made up besluitvorming:aanmaakdatum
                (:retracted           :boolean  ,(s-prefix "besluitvorming:ingetrokken")) ;; NOTE: What is the URI of property 'ingetrokken'? Made up besluitvorming:ingetrokken
                (:priority            :number   ,(s-prefix "ext:prioriteit"))
                (:for-press           :boolean  ,(s-prefix "ext:forPress"))
                (:show-in-newsletter  :boolean  ,(s-prefix "ext:toonInKortBestek"))
                (:record              :string   ,(s-prefix "besluitvorming:notulen")) ;; NOTE: What is the URI of property 'notulen'? Made up besluitvorming:notulen
                (:title-press         :string   ,(s-prefix "besluitvorming:titelPersagenda"))
                (:explanation         :string   ,(s-prefix "ext:toelichting"))
                (:text-press          :string   ,(s-prefix "besluitvorming:tekstPersagenda"))
                ;; Added properties from subcases
                (:short-title         :string   ,(s-prefix "dct:alternative"))
                (:title               :string   ,(s-prefix "dct:title"))
                (:modified            :datetime ,(s-prefix "ext:modified"))
                (:formally-ok         :url      ,(s-prefix "ext:formeelOK"))
                (:show-as-remark      :boolean  ,(s-prefix "ext:wordtGetoondAlsMededeling"))
                (:is-approval         :boolean  ,(s-prefix "ext:isGoedkeuringVanDeNotulen"))) ;; NOTE: What is the URI of property 'titelPersagenda'? Made up besluitvorming:titelPersagenda
  :has-one `((agendaitem              :via      ,(s-prefix "besluit:aangebrachtNa")
                                      :as "previous-agenda-item")
             (user                    :via      ,(s-prefix "ext:modifiedBy")
                                      :as "modified-by")
             (agenda                  :via      ,(s-prefix "dct:hasPart")
                                      :inverse t
                                      :as "agenda")
             (meeting-record          :via      ,(s-prefix "ext:notulenVanAgendaPunt")
                                      :as "meeting-record")
             (agenda-activity         :via      ,(s-prefix "besluitvorming:genereertAgendapunt")
                                      :inverse t
                                      :as "agenda-activity"))
  :has-many `((remark                 :via      ,(s-prefix "besluitvorming:opmerking") ;; NOTE: opmerkingEN would be more suitable?
                                      :as "remarks")
              (agenda-item-treatment  :via        ,(s-prefix "besluitvorming:heeftOnderwerp")
                                      :inverse t
                                      :as "treatments")
            ;; Added has-many relations from subcases
              (approval               :via      ,(s-prefix "ext:agendapuntGoedkeuring")
                                      :as "approvals")
              (mandatee               :via      ,(s-prefix "ext:heeftBevoegdeVoorAgendapunt") ;; NOTE: used mandataris instead of agent
                                      :as "mandatees")
              (piece                  :via      ,(s-prefix "besluitvorming:geagendeerdStuk")
                                      :as "pieces")
              (piece                  :via ,(s-prefix "ext:bevatReedsBezorgdAgendapuntDocumentversie") ;; NOTE: instead of dct:hasPart (mu-cl-resources relation type checking workaround)
                                      :as "linked-pieces"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/agendapunten/")
  :features `(no-pagination-defaults include-uri)
  :on-path "agendaitems")


  (define-resource approval ()
  :class (s-prefix "ext:Goedkeuring")
  :properties `((:created   :datetime ,(s-prefix "ext:aangemaakt")))
  :has-one `((mandatee      :via ,(s-prefix "ext:goedkeuringen")
                            :inverse t
                            :as "mandatee")
             (agendaitem    :via ,(s-prefix "ext:agendapuntGoedkeuring")
                            :inverse t
                            :as "agendaitem"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/goedkeuringen/")
  :on-path "approvals")

(define-resource agenda-item-treatment ()
  :class (s-prefix "besluit:BehandelingVanAgendapunt") ; Also includes properties/relations from besluitvorming:Beslissingsactiviteit
  :properties `(
                (:created     :datetime       ,(s-prefix "dct:created"))
                (:modified    :datetime   ,(s-prefix "dct:modified"))
                )
  :has-many `(
              ; Omdat de mu-cl-resources configuratie momenteel onze meest accurate documentatie is over huidig model / huidige data, laat ik 'm er toch graag in. Dit predicaat is in-data veel aanwezig (en waardevolle data), en zal in de toekomst terug opgepikt worden
              ; (piece      :via ,(s-prefix "ext:documentenVoorBeslissing")
              ;                :as "pieces")
              )
  :has-one `((agendaitem            :via        ,(s-prefix "besluitvorming:heeftOnderwerp")
                                    :as "agendaitem")
             (subcase               :via        ,(s-prefix "ext:beslissingVindtPlaatsTijdens")
                                    :as "subcase")
             (document-container    :via        ,(s-prefix "besluitvorming:genereertVerslag") ; "beslissingsfiche", Property from besluitvorming:Beslissingsactiviteit
                                    :as "report")
             (newsletter-info       :via        ,(s-prefix "prov:generated")
                                    :as "newsletter-info")
             (decision-result-code  :via        ,(s-prefix "besluitvorming:resultaat")
                                    :as "decision-result-code"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/behandelingen-van-agendapunt/")
  :features '(include-uri)
  :on-path "agenda-item-treatments")

(define-resource decision-result-code ()
  :class (s-prefix "ext:BeslissingsResultaatCode")
  :properties `(
                (:label       :string ,(s-prefix "skos:prefLabel"))
                (:priority    :number ,(s-prefix "ext:priority"))
               )
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/concept/beslissings-resultaat-codes/")
  :features '(include-uri)
  :on-path "decision-result-codes")

(define-resource government-unit ()
  :class (s-prefix "besluit:Bestuurseenheid")
  :properties `((:name :string ,(s-prefix "skos:prefLabel")))
  :has-one `((jurisdiction-area :via ,(s-prefix "besluit:werkingsgebied")
                                   :as "area-of-jurisdiction")
             (government-unit-classification-code :via ,(s-prefix "besluit:classificatie")
                                                  :as "classification"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/bestuurseenheden/")
  :features '(include-uri)
  :on-path "government-units")

;; Unmodified from lblod/loket
(define-resource jurisdiction-area ()
  :class (s-prefix "prov:Location")
  :properties `((:name :string ,(s-prefix "rdfs:label"))
                (:level :string, (s-prefix "ext:werkingsgebiedNiveau")))
  :has-many `((government-unit :via ,(s-prefix "besluit:werkingsgebied")
                               :inverse t
                               :as "government-units"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/werkingsgebieden/")
  :features '(include-uri)
  :on-path "jurisdiction-areas")

;; Unmodified from lblod/loket
(define-resource government-unit-classification-code ()
  :class (s-prefix "ext:BestuurseenheidClassificatieCode")
  :properties `((:label :string ,(s-prefix "skos:prefLabel"))
                (:scope-note :string ,(s-prefix "skos:scopeNote"))
                (:alt-label :string ,(s-prefix "skos:altLabel")))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/concept/bestuurseenheid-classificatie-codes/")
  :features '(include-uri)
  :on-path "government-unit-classification-codes")

(define-resource meeting ()
  :class (s-prefix "besluit:Vergaderactiviteit")
  :properties `((:planned-start         :datetime ,(s-prefix "besluit:geplandeStart"))
                (:started-on            :datetime ,(s-prefix "prov:startedAtTime")) ;; NOTE: Kept ':geplande-start' from besluit instead of ':start' from besluitvorming
                (:ended-on              :datetime ,(s-prefix "prov:endedAtTime")) ;; NOTE: Kept ':geeindigd-op-tijdstip' from besluit instead of ':eind' from besluitvorming
                (:location              :string   ,(s-prefix "prov:atLocation"))
                (:released-decisions    :datetime ,(s-prefix "ext:releasedDecisions"))
                (:released-documents    :datetime ,(s-prefix "ext:releasedDocuments"))
                (:number                :number   ,(s-prefix "adms:identifier"))
                (:is-final              :boolean  ,(s-prefix "ext:finaleZittingVersie")) ;; 2019-01-09: Also see note on agenda "is-final". "ext:finaleZittingVersie" == true means "agenda afgesloten" but not at a version level
                (:kind                  :url      ,(s-prefix "dct:type"))
                (:extra-info            :string   ,(s-prefix "ext:extraInfo"))
                (:number-representation :string   ,(s-prefix "ext:numberRepresentation")))
  :has-many `((agenda                   :via      ,(s-prefix "besluitvorming:isAgendaVoor")
                                        :inverse t
                                        :as "agendas")
              (subcase                  :via      ,(s-prefix "besluitvorming:isAangevraagdVoor")
                                        :inverse t
                                        :as "requested-subcases")
              (piece                    :via      ,(s-prefix "ext:zittingDocumentversie")
                                        :as "pieces"))
  :has-one `((agenda                    :via      ,(s-prefix "besluitvorming:behandelt");; NOTE: What is the URI of property 'behandelt'? Made up besluitvorming:behandelt
                                        :as "agenda")
             (meeting-record            :via      ,(s-prefix "ext:algemeneNotulen")
                                        :as "notes")
             (newsletter-info           :via      ,(s-prefix "ext:algemeneNieuwsbrief")
                                        :as "newsletter")
             (signature                 :via      ,(s-prefix "ext:heeftHandtekening")
                                        :as "signature")
             (mail-campaign             :via      ,(s-prefix "ext:heeftMailCampagnes")
                                        :as "mail-campaign"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/zittingen/")
  :features '(include-uri)
  :on-path "meetings")

(define-resource meeting-record ()
  :class (s-prefix "ext:Notule")
  :properties `((:modified      :datetime     ,(s-prefix "ext:aangepast"))
                (:created       :datetime     ,(s-prefix "ext:aangemaaktOp"))
                (:announcements :string   ,(s-prefix "ext:mededelingen"))
                (:others        :string   ,(s-prefix "ext:varia"))
                (:description   :string   ,(s-prefix "ext:description"))
                (:richtext      :string   ,(s-prefix "ext:htmlInhoud")))
  :has-one `((meeting           :via      ,(s-prefix "ext:algemeneNotulen")
                                :inverse t
                                :as "meeting")
             (agendaitem        :via      ,(s-prefix "ext:notulenVanAgendaPunt")
                                :inverse t
                                :as "agendaitem")
             (document-container  :via      ,(s-prefix "ext:getekendeNotulen")
                                :as "signed-document-container"))
  :has-many `((mandatee        :via      ,(s-prefix "ext:aanwezigen")
                                :as "attendees")
              (piece         :via      ,(s-prefix "ext:getekendeDocumentVersiesVoorNotulen")
                                :as "pieces"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/notulen/")
  :features '(include-uri)
  :on-path "meeting-records")
