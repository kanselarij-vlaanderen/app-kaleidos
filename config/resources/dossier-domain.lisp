(define-resource case ()
  :class (s-prefix "dossier:Dossier")
  :properties `((:created       :datetime ,(s-prefix "dct:created"))
                (:short-title   :string   ,(s-prefix "dct:alternative"))
                (:number        :number   ,(s-prefix "adms:identifier")) ;; NOTE: only for legacy, do we want this ??
                (:is-archived   :boolean  ,(s-prefix "ext:isGearchiveerd"))
                (:title         :string   ,(s-prefix "dct:title"))
                (:confidential  :boolean  ,(s-prefix "ext:vertrouwelijk"))
)
  :has-many `((subcase          :via      ,(s-prefix "dossier:doorloopt")
                                :as "subcases"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/dossiers/")
  :features '(include-uri)
  :on-path "cases")

(define-resource case-type ()
  :class (s-prefix "ext:DossierTypeCode")
  :properties `((:label       :string ,(s-prefix "skos:prefLabel"))
                (:scope-note  :string ,(s-prefix "skos:scopeNote"))
                (:alt-label   :string ,(s-prefix "skos:altLabel"))
                (:deprecated  :boolean ,(s-prefix "owl:deprecated")))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/concept/dossier-type-codes/")
  :features '(include-uri)
  :on-path "case-types")

(define-resource subcase ()
  :class (s-prefix "dossier:Procedurestap")
  :properties `((:short-title         :string ,(s-prefix "dct:alternative"))
                (:title               :string ,(s-prefix "dct:title"))
                (:is-archived         :boolean   ,(s-prefix "ext:isProcedurestapGearchiveerd"))
                (:confidential        :boolean   ,(s-prefix "ext:vertrouwelijk"))
                (:subcase-name        :string ,(s-prefix "ext:procedurestapNaam"))
                (:created             :datetime ,(s-prefix "dct:created"))
                (:modified            :datetime ,(s-prefix "ext:modified"))
                (:show-as-remark      :boolean ,(s-prefix "ext:wordtGetoondAlsMededeling"))
  :has-one `((case                    :via ,(s-prefix "dossier:doorloopt")
                                      :inverse t
                                      :as "case")
             (meeting                 :via ,(s-prefix "ext:isAangevraagdVoor")
                                      :as "requested-for-meeting")
             (access-level            :via ,(s-prefix "ext:toegangsniveauVoorProcedurestap")
                                      :as "access-level")
             (subcase-type            :via ,(s-prefix "dct:type")
                                      :as "type")
             (mandatee                :via ,(s-prefix "ext:indiener")
                                      :as "requested-by")
             (user                    :via      ,(s-prefix "ext:modifiedBy")
                                      :as "modified-by"))
  :has-many `((mandatee               :via ,(s-prefix "ext:heeftBevoegde") ;; NOTE: used mandataris instead of agent
                                      :as "mandatees")
              (document               :via ,(s-prefix "ext:bevatDocumentversie") ;; NOTE: instead of dct:hasPart (mu-cl-resources relation type checking workaround)
                                      :as "document-versions")
              (document               :via ,(s-prefix "ext:bevatReedsBezorgdeDocumentversie") ;; NOTE: instead of dct:hasPart (mu-cl-resources relation type checking workaround)
                                      :as "linked-document-versions")
              (ise-code               :via ,(s-prefix "ext:heeftInhoudelijkeStructuurElementen")
                                      :as "ise-codes")
              (agenda-activity        :via ,(s-prefix "besluitvorming:vindtPlaatsTijdens") ;; TODO: but others as wel. mu-cl-resources polymorphism limitation. Rename to agenderingVindtPlaatsTijdens ?
                                      :inverse t
                                      :as "agenda-activities")
              (agenda-item-treatment  :via ,(s-prefix "ext:beslissingVindtPlaatsTijdens") ;; mu-cl-resources polymorphism limitation. vindtPlaatsTijdens can only be used once !
                                      :inverse t
                                      :as "agenda-item-treatments"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/procedurestappen/")
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
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/concept/procedurestap-types/")
  :features '(include-uri)
  :on-path "subcase-types")

  (define-resource agenda-activity ()
  :class (s-prefix "besluitvorming:Agendering")
  :properties `((:start-date      :datetime ,(s-prefix "dossier:startDatum")))
  :has-one `((subcase             :via ,(s-prefix "besluitvorming:vindtPlaatsTijdens")
                                  :as "subcase"))
  :has-many `((agendaitem         :via ,(s-prefix "besluitvorming:genereertAgendapunt")
                                  :as "agendaitems"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/agenderingen/")
  :features '(include-uri)
  :on-path "agenda-activities")

(define-resource access-level ()
  :class (s-prefix "ext:ToegangsniveauCode") ;; NOTE: as well as skos:Concept
  :properties `((:label       :string ,(s-prefix "skos:prefLabel"))
                (:scope-note  :string ,(s-prefix "skos:scopeNote"))
                (:alt-label   :string ,(s-prefix "skos:altLabel"))
                (:priority    :string ,(s-prefix "ext:prioriteit")))
  :has-many `((subcase        :via ,(s-prefix "ext:toegangsniveauVoorProcedurestap")
                              :inverse t
                              :as "subcases")
              (document-container  :via ,(s-prefix "ext:toegangsniveauVoorDocument") ;; 2019-01-09 : deprecated. To be removed after checking frontend dependency
                              :inverse t
                              :as "documents")
              (case           :via ,(s-prefix "ext:toegangsniveauVoorDossier")
                              :inverse t
                              :as "cases"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/concept/toegangs-niveaus/")
  :features '(include-uri)
  :on-path "access-levels")

(define-resource person-or-organization ()
  :class (s-prefix "ext:PersonOrOrganization") ;; NOTE: as resource hack for super typing, is person or organization
  :properties `((:type :string ,(s-prefix "rdfs:type")))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/personen/") ;; NOTE: Should in theory never get used, as this is a read-only hack.
  :features '(include-uri)
  :on-path "person-or-organization")
