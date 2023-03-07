(define-resource agenda ()
  :class (s-prefix "besluitvorming:Agenda")
  :properties `((:issued      :datetime   ,(s-prefix "dct:issued"))
                (:title        :string     ,(s-prefix "dct:title"))
                (:serialnumber :string    ,(s-prefix "besluitvorming:volgnummer"))
                (:created     :datetime       ,(s-prefix "dct:created"))
                (:modified    :datetime   ,(s-prefix "dct:modified")))
  :has-one `((meeting         :via        ,(s-prefix "besluitvorming:isAgendaVoor")
                              :as "created-for")
             (meeting         :via        ,(s-prefix "besluitvorming:behandelt")
                              :inverse t
                              :as "meeting")
             (agendastatus    :via        ,(s-prefix "besluitvorming:agendaStatus")
                              :as "status")
             (agenda          :via        ,(s-prefix "prov:wasRevisionOf")
                              :as "previous-version")
             (agenda           :via        ,(s-prefix "prov:wasRevisionOf")
                               :inverse t
                               :as "next-version"))
  :has-many `((agendaitem     :via        ,(s-prefix "dct:hasPart")
                              :as "agendaitems")
             (agendaactivities :via     ,(s-prefix "prov:Activity")
                              :inverse t
                              :as "status-activities")               )
  :resource-base (s-url "http://themis.vlaanderen.be/id/agenda/")
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
  :resource-base (s-url "http://themis.vlaanderen.be/id/agenda-status/")
  :features '(include-uri)
  :on-path "agendastatuses")

(define-resource agendaitem ()
  :class (s-prefix "besluit:Agendapunt")
  :properties `((:created             :datetime ,(s-prefix "dct:created"))
                ;; (:retracted           :boolean  ,(s-prefix "besluitvorming:ingetrokken")) ;; still exists in legacy until we sort inconsistensies in data
                (:number              :integer  ,(s-prefix "schema:position"))
                (:for-press           :boolean  ,(s-prefix "ext:forPress"))
                ;; (:title-press         :string   ,(s-prefix "besluitvorming:titelPersagenda")) NOTE: this property is unused, but data is still available in the database
                (:comment             :string   ,(s-prefix "schema:comment"))
                (:private-comment     :string   ,(s-prefix "ext:privateComment"))
                ;; (:text-press          :string   ,(s-prefix "besluitvorming:tekstPersagenda")) NOTE: this property is unused, but data is still available in the database
                ;; Added properties from subcases
                (:short-title         :string   ,(s-prefix "besluitvorming:korteTitel"))
                (:title               :string   ,(s-prefix "dct:title"))
                (:modified            :datetime ,(s-prefix "dct:modified"))
                (:formally-ok         :url      ,(s-prefix "ext:formeelOK"))
                (:is-approval         :boolean  ,(s-prefix "ext:isGoedkeuringVanDeNotulen")))
  :has-one `((agendaitem              :via      ,(s-prefix "besluit:aangebrachtNa")
                                      :as "previous-agenda-item")
             (user                    :via      ,(s-prefix "ext:modifiedBy")
                                      :as "modified-by")
             (agenda                  :via      ,(s-prefix "dct:hasPart")
                                      :inverse t
                                      :as "agenda")
             (agendaitem              :via        ,(s-prefix "prov:wasRevisionOf")
                                      :as "previous-version")
             (agendaitem              :via        ,(s-prefix "prov:wasRevisionOf")
                                      :inverse t
                                      :as "next-version")
             (agenda-activity         :via      ,(s-prefix "besluitvorming:genereertAgendapunt")
                                      :inverse t
                                      :as "agenda-activity")
             (agenda-item-treatment   :via        ,(s-prefix "dct:subject")
                                      :inverse t
                                      :as "treatment")
             (concept                 :via ,(s-prefix "dct:type")
                                      :as "type"))
  :has-many `(
            ;; Added has-many relations from subcases
              (mandatee               :via      ,(s-prefix "ext:heeftBevoegdeVoorAgendapunt") ;; NOTE: used mandataris instead of agent
                                      :as "mandatees")
              (piece                  :via      ,(s-prefix "besluitvorming:geagendeerdStuk")
                                      :as "pieces")
              (piece                  :via ,(s-prefix "ext:bevatReedsBezorgdAgendapuntDocumentversie") ;; NOTE: instead of dct:hasPart (mu-cl-resources relation type checking workaround)
                                      :as "linked-pieces"))
  :resource-base (s-url "http://themis.vlaanderen.be/id/agendapunt/")
  :features `(no-pagination-defaults include-uri)
  :on-path "agendaitems")

(define-resource agenda-item-treatment ()
  :class (s-prefix "besluit:BehandelingVanAgendapunt")
  :properties `(
                (:created     :datetime   ,(s-prefix "dct:created"))
                (:modified    :datetime   ,(s-prefix "dct:modified"))
                )
  :has-one `((decision-activity     :via ,(s-prefix "besluitvorming:heeftBeslissing"),
                                    :as "decision-activity")
             (news-item             :via ,(s-prefix "prov:wasDerivedFrom")
                                    :inverse t
                                    :as "news-item")
            )
  :has-many `(
             ;; agenda-item-treatment has multiple agenda-items in the sense that there is one
             ;; agenda-item version per version of the agenda
             (agendaitem            :via        ,(s-prefix "dct:subject")
                                    :as "agendaitems"))
  :resource-base (s-url "http://themis.vlaanderen.be/id/behandeling-van-agendapunt/")
  :features '(include-uri)
  :on-path "agenda-item-treatments")


(define-resource decision-activity ()
  :class (s-prefix "besluitvorming:Beslissingsactiviteit")
  :properties `((:start-date  :date     ,(s-prefix "dossier:Activiteit.startdatum")))
  :has-one `((subcase               :via        ,(s-prefix "ext:beslissingVindtPlaatsTijdens")
                                    :as "subcase")
             (piece                 :via        ,(s-prefix "besluitvorming:beschrijft")
                                    :inverse t
                                    :as "report")
             (agenda-item-treatment :via        ,(s-prefix "besluitvorming:heeftBeslissing")
                                    :inverse t
                                    :as "treatment")
             (concept               :via        ,(s-prefix "besluitvorming:resultaat")
                                    :as "decision-result-code")
            )
  :has-many `(
              ; Omdat de mu-cl-resources configuratie momenteel onze meest accurate documentatie is over huidig model / huidige data, laat ik 'm er toch graag in. Dit predicaat is in-data veel aanwezig (en waardevolle data), en zal in de toekomst terug opgepikt worden
              ; (piece      :via ,(s-prefix "prov:used")
              ;                :as "pieces")
              (publication-flow     :via ,(s-prefix "dct:subject"),
                                    :inverse t
                                    :as "publication-flows")
              (sign-flow            :via ,(s-prefix "sign:heeftBeslissing"),
                                    :inverse t
                                    :as "sign-flows")
            )
  :resource-base (s-url "http://themis.vlaanderen.be/id/beslissingsactiviteit/")
  :features '(include-uri)
  :on-path "decision-activities")

(define-resource government-unit ()
  :class (s-prefix "besluit:Bestuurseenheid")
  :properties `((:name :string ,(s-prefix "skos:prefLabel")))
  :has-one `((jurisdiction-area :via ,(s-prefix "besluit:werkingsgebied")
                                   :as "area-of-jurisdiction")
             (government-unit-classification-code :via ,(s-prefix "besluit:classificatie")
                                                  :as "classification"))
  :resource-base (s-url "http://themis.vlaanderen.be/id/bestuurseenheid/")
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
  :resource-base (s-url "http://themis.vlaanderen.be/id/werkingsgebied/")
  :features '(include-uri)
  :on-path "jurisdiction-areas")

;; Unmodified from lblod/loket
(define-resource government-unit-classification-code ()
  :class (s-prefix "ext:BestuurseenheidClassificatieCode")
  :properties `((:label :string ,(s-prefix "skos:prefLabel"))
                (:scope-note :string ,(s-prefix "skos:scopeNote"))
                (:alt-label :string ,(s-prefix "skos:altLabel")))
  :resource-base (s-url "http://themis.vlaanderen.be/id/concept/bestuurseenheid-classificatie-code/")
  :features '(include-uri)
  :on-path "government-unit-classification-codes")

(define-resource meeting ()
  :class (s-prefix "besluit:Vergaderactiviteit")
  :properties `((:planned-start         :datetime ,(s-prefix "besluit:geplandeStart"))
                (:started-on            :datetime ,(s-prefix "prov:startedAtTime")) ;; NOTE: Kept ':geplande-start' from besluit instead of ':start' from besluitvorming
                (:ended-on              :datetime ,(s-prefix "prov:endedAtTime")) ;; NOTE: Kept ':geeindigd-op-tijdstip' from besluit instead of ':eind' from besluitvorming
                (:location              :string   ,(s-prefix "prov:atLocation"))
                (:number                :number   ,(s-prefix "adms:identifier")) ;; currently mixed types (xsd:decimal & xsd:integer) exist in prod db
                (:extra-info            :string   ,(s-prefix "ext:extraInfo"))
                (:number-representation :string   ,(s-prefix "ext:numberRepresentation")))
  :has-many `((agenda                   :via      ,(s-prefix "besluitvorming:isAgendaVoor") ;; All agenda versions, including the final version
                                        :inverse t
                                        :as "agendas")
              (piece                    :via      ,(s-prefix "ext:zittingDocumentversie")
                                        :as "pieces")
              (themis-publication-activity :via   ,(s-prefix "prov:used")
                                           :inverse t
                                           :as "themis-publication-activities"))
  :has-one `((agenda                    :via      ,(s-prefix "besluitvorming:behandelt") ;; Final agenda version that is treatened during the meeting
                                        :as "agenda")
              ;; (piece                    :via ,(s-prefix "dossier:genereert") ;; this relation exists in legacy data, but we do not show this in the frontend currently
              ;;                           :as "notes") ;; note: is this a hasOne or hasMany ?
             (mail-campaign             :via      ,(s-prefix "ext:heeftMailCampagnes")
                                        :as "mail-campaign")
             (concept                   :via      ,(s-prefix "dct:type")
                                        :as "kind")
             (meeting                   :via      ,(s-prefix "dct:isPartOf")
                                        :as "main-meeting")
             (internal-decision-publication-activity :via  ,(s-prefix "ext:internalDecisionPublicationActivityUsed")
                                        :inverse t
                                        :as "internal-decision-publication-activity")
             (internal-document-publication-activity :via  ,(s-prefix "ext:internalDocumentPublicationActivityUsed")
                                        :inverse t
                                        :as "internal-document-publication-activity"))
  :resource-base (s-url "http://themis.vlaanderen.be/id/zitting/")
  :features '(include-uri)
  :on-path "meetings")
