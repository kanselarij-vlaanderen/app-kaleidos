(define-resource case ()
  :class (s-prefix "dossier:Dossier")
  :properties `((:title         :string   ,(s-prefix "dct:title"))
                (:short-title   :string   ,(s-prefix "dct:alternative"))
                (:created       :datetime ,(s-prefix "dct:created"))
                (:number        :number   ,(s-prefix "adms:identifier")))
  :has-one `((decisionmaking-flow  :via      ,(s-prefix "dossier:Dossier.isNeerslagVan")
                    :as "decisionmaking-flow")
             (parliament-flow  :via      ,(s-prefix "parl:behandeltDossier")
                    :inverse t
                    :as "parliament-flow"))
  :has-many `((piece             :via      ,(s-prefix "dossier:Dossier.bestaatUit")
                                 :as "pieces")
              (publication-flow  :via      ,(s-prefix "dossier:behandelt")
                                 :inverse t
                                 :as "publication-flows")
              (sign-flow         :via      ,(s-prefix "sign:behandeltDossier")
                                 :inverse t
                                 :as "sign-flows"))

  :resource-base (s-url "http://themis.vlaanderen.be/id/dossier/")
  :features '(include-uri)
  :on-path "cases")

(define-resource decisionmaking-flow ()
  :class (s-prefix "besluitvorming:Besluitvormingsaangelegenheid")
  :properties `((:title         :string   ,(s-prefix "dct:title")) ;; Both title and short-title are unused for now, we always use the titles from the linked case
                (:short-title   :string   ,(s-prefix "dct:alternative"))
                (:opened        :datetime ,(s-prefix "besluitvorming:openingsdatum"))
                (:closed        :datetime ,(s-prefix "besluitvorming:sluitingsdatum")))
  :has-one `((case               :via      ,(s-prefix "dossier:Dossier.isNeerslagVan")
                                 :inverse t
                                 :as "case"))
  :has-many `((subcase           :via      ,(s-prefix "dossier:doorloopt")
                                 :as "subcases")
              (concept           :via ,(s-prefix "besluitvorming:beleidsveld") ;; NOTE: Contains both Beleidsveld and Beleidsdomein, despite the predicate name. These are synced to the subcase government-areas
                                 :as "government-areas")
              (submission        :via ,(s-prefix "subm:ingediendVoor")
                                 :inverse t
                                 :as "submissions"))

  :resource-base (s-url "http://themis.vlaanderen.be/id/besluitvormingsaangelegenheid/")
  :features '(include-uri)
  :on-path "decisionmaking-flows")

(define-resource case-type ()
  :class (s-prefix "ext:DossierTypeCode")
  :properties `((:label       :string ,(s-prefix "skos:prefLabel"))
                (:scope-note  :string ,(s-prefix "skos:scopeNote"))
                (:alt-label   :string ,(s-prefix "skos:altLabel"))
                (:deprecated  :boolean ,(s-prefix "owl:deprecated")))
  :resource-base (s-url "http://themis.vlaanderen.be/id/concept/dossier-type/")
  :features '(include-uri)
  :on-path "case-types")

(define-resource subcase ()
  :class (s-prefix "dossier:Procedurestap")
  :properties `((:short-title         :string ,(s-prefix "dct:alternative"))
                (:title               :string ,(s-prefix "dct:title"))
                (:confidential        :boolean   ,(s-prefix "ext:vertrouwelijk")) ;; This is seen as "Beperkte toegang" in frontend
                (:subcase-name        :string ,(s-prefix "ext:procedurestapNaam"))
                (:created             :datetime ,(s-prefix "dct:created"))
                (:modified            :datetime ,(s-prefix "ext:modified"))
                (:agenda-activity-number :integer ,(s-prefix "adms:identifier")))
  :has-one `((decisionmaking-flow     :via ,(s-prefix "dossier:doorloopt")
                                      :inverse t
                                      :as "decisionmaking-flow")
             (subcase-type            :via ,(s-prefix "dct:type")
                                      :as "type")
             (concept                 :via ,(s-prefix "ext:agendapuntType")
                                      :as "agenda-item-type")
             (mandatee                :via ,(s-prefix "ext:indiener")
                                      :as "requested-by")
             (user                    :via ,(s-prefix "ext:modifiedBy")
                                      :as "modified-by")
             (parliament-retrieval-activity :via ,(s-prefix "prov:generated")
                                            :inverse t
                                            :as "parliament-retrieval-activity")
             (piece                   :via ,(s-prefix "ext:heeftBekrachtiging")
                                      :as "ratification")
             (submission-internal-review    :via ,(s-prefix "subm:isInterneBeoordelingVoor")
                                            :inverse t
                                            :as "internal-review"))
  :has-many `((mandatee               :via ,(s-prefix "ext:heeftBevoegde") ;; NOTE: used mandataris instead of agent
                                      :as "mandatees")
              (mandatee               :via ,(s-prefix "ext:bekrachtigdDoor")
                                      :as "ratified-by")
              (piece                  :via ,(s-prefix "ext:bevatReedsBezorgdeDocumentversie") ;; NOTE: instead of dct:hasPart (mu-cl-resources relation type checking workaround)
                                      :as "linked-pieces")
              (agenda-activity        :via ,(s-prefix "besluitvorming:vindtPlaatsTijdens") ;; TODO: but others as wel. mu-cl-resources polymorphism limitation. Rename to agenderingVindtPlaatsTijdens ?
                                      :inverse t
                                      :as "agenda-activities")
              (submission-activity    :via ,(s-prefix "ext:indieningVindtPlaatsTijdens") ;; subpredicate for besluitvorming:vindtPlaatsTijdens
                                      :inverse t
                                      :as "submission-activities")
              ;; - mu-cl-resources polymorphism limitation. vindtPlaatsTijdens can only be used once !
              ;; - This relationship is has-many because it is assumed that a subcase can be put on agenda,
              ;;   postponed (postponing is a decision) and subsequently be decided upon again, thereby adding
              ;;   another decision-activity
              (decision-activity      :via ,(s-prefix "ext:beslissingVindtPlaatsTijdens")
                                      :inverse t
                                      :as "decision-activities")
              (concept                :via ,(s-prefix "besluitvorming:beleidsveld")
                                      :as "government-areas")
              (submission             :via ,(s-prefix "subm:ingediendVoorProcedurestap")
                                      :inverse t
                                      :as "submissions"))
  :resource-base (s-url "http://themis.vlaanderen.be/id/procedurestap/")
  :features '(include-uri)
  :on-path "subcases")

(define-resource subcase-type () ;; NOTE: Should be subclass of besluitvorming:Status (mu-cl-resources reasoner workaround)
  :class (s-prefix "ext:ProcedurestapType") ;; NOTE: as well as skos:Concept
  :properties `((:label           :string ,(s-prefix "skos:prefLabel"))
                (:scope-note      :string ,(s-prefix "skos:scopeNote"))
                (:alt-label       :string ,(s-prefix "skos:altLabel")))
  :has-many `((subcase            :via ,(s-prefix "dct:type")
                                  :inverse t
                                  :as "subcases"))
  :resource-base (s-url "http://themis.vlaanderen.be/id/concept/procedurestap-type/")
  :features '(include-uri)
  :on-path "subcase-types")

(define-resource agenda-activity (activity)
  :class (s-prefix "besluitvorming:Agendering")
  :properties `((:start-date      :datetime ,(s-prefix "dossier:startDatum"))) ;; should be dossier:Activiteit.startdatum
  :has-one `((subcase             :via ,(s-prefix "besluitvorming:vindtPlaatsTijdens")
                                  :as "subcase"))
  :has-many `((agendaitem         :via ,(s-prefix "besluitvorming:genereertAgendapunt")
                                  :as "agendaitems")
              (submission-activity :via ,(s-prefix "prov:wasInformedBy")
                                   :as "submission-activities"))
  :resource-base (s-url "http://themis.vlaanderen.be/id/agendering/")
  :features '(include-uri)
  :on-path "agenda-activities")

(define-resource submission-activity (activity)
  :class (s-prefix "ext:Indieningsactiviteit")
  :properties `((:start-date      :datetime ,(s-prefix "dossier:Activiteit.startdatum")))
  :has-one `((subcase             :via ,(s-prefix "ext:indieningVindtPlaatsTijdens") ;; subpredicate for besluitvorming:vindtPlaatsTijdens
                                  :as "subcase")
             (submission          :via ,(s-prefix "subm:ingediendAls")
                                  :as "submission")
             (agenda-activity     :via ,(s-prefix "prov:wasInformedBy")
                                  :inverse t
                                  :as "agenda-activity"))
  :has-many `((piece              :via ,(s-prefix "prov:generated")
                                  :as "pieces")
              (mandatee           :via ,(s-prefix "prov:qualifiedAssociation")
                                  :as "submitters"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/indieningsactiviteit/")
  :features '(include-uri)
  :on-path "submission-activities")
