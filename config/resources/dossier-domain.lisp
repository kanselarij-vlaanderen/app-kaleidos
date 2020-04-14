(define-resource case ()
  :class (s-prefix "dbpedia:Case")
  :properties `((:created       :datetime ,(s-prefix "dct:created")) ;; NOTE: Type should be :date instead?
                (:short-title   :string   ,(s-prefix "dct:alternative"))
                (:number        :number   ,(s-prefix "adms:identifier")) ;; NOTE: Type should be :number instead?
                (:is-archived   :boolean   ,(s-prefix "ext:isGearchiveerd"))
                (:title         :string   ,(s-prefix "dct:title"))
                (:confidential  :boolean  ,(s-prefix "ext:vertrouwelijk"))
                (:freeze-access-level :boolean ,(s-prefix "ext:freezeAccessLevel")) ;; deprecated
)
  :has-one `((access-level   :via ,(s-prefix "ext:toegangsniveauVoorDossier")
                                :as "access-level")
             (case-type         :via      ,(s-prefix "dct:type")
                                :as "type")
             (policy-level      :via      ,(s-prefix "ext:heeftBeleidsNiveau")
                                :as "policy-level")
             (meeting           :via      ,(s-prefix "ext:heeftBijbehorendeDossiers")
                                :inverse t
                                :as "related-meeting"))
  :has-many `((remark           :via      ,(s-prefix "besluitvorming:opmerking")
                                :as "opmerking") ;; NOTE: opmerkingEN would be more suitable?
              (person           :via      ,(s-prefix "besluitvorming:heeftIndiener") ;; NOTE: used persoon instead of agent
                                :as "creators")
              (person           :via      ,(s-prefix "besluitvorming:heeftContactpersoon") ;; NOTE: used persoon instead of agent
                                :as "contactPersons")
              (subcase          :via      ,(s-prefix "dct:hasPart")
                                :as "subcases")
              (case             :via      ,(s-prefix "dct:relation")
                                :as "related"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/dossiers/")
  :features '(include-uri)
  :on-path "cases")

(define-resource case-type ()
  :class (s-prefix "ext:DossierTypeCode")
  :properties `((:label       :string ,(s-prefix "skos:prefLabel"))
                (:scope-note  :string ,(s-prefix "skos:scopeNote"))
                (:alt-label   :string ,(s-prefix "skos:altLabel"))
                (:deprecated  :boolean ,(s-prefix "owl:deprecated")))
  :has-many `((case           :via ,(s-prefix "dct:type")
                              :inverse t
                              :as "cases"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/concept/dossier-type-codes/")
  :features '(include-uri)
  :on-path "case-types")

(define-resource policy-level ()
  :class (s-prefix "ext:BeleidsNiveau")
  :properties `((:label       :string ,(s-prefix "skos:prefLabel"))
                (:scope-note  :string ,(s-prefix "skos:scopeNote"))
                (:alt-label   :string ,(s-prefix "skos:altLabel")))
  :has-many `((case            :via ,(s-prefix "ext:heeftBeleidsNiveau")
                              :inverse t
                              :as "cases"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/concept/beleidsniveaus/")
  :features '(include-uri)
  :on-path "policy-levels")

(define-resource subcase ()
  :class (s-prefix "dbpedia:UnitOfWork")
  :properties `((:short-title         :string ,(s-prefix "dct:alternative"))
                (:title               :string ,(s-prefix "dct:title"))
                (:subcase-identifier  :string ,(s-prefix "ext:procedurestapNummer"))
                (:is-archived         :boolean   ,(s-prefix "ext:isProcedurestapGearchiveerd"))
                (:confidential        :boolean   ,(s-prefix "ext:vertrouwelijk"))
                (:formally-ok         :boolean  ,(s-prefix "besluitvorming:formeelOK")) ;; NOTE: What is the URI of property 'formeelOK'? Made up besluitvorming:formeelOK
                (:subcase-name        :string ,(s-prefix "ext:procedurestapNaam"))
                (:created             :datetime ,(s-prefix "dct:created"))
                (:modified            :datetime ,(s-prefix "ext:modified"))
                (:concluded           :boolean ,(s-prefix "besluitvorming:besloten"))
                (:show-as-remark      :boolean ,(s-prefix "ext:wordtGetoondAlsMededeling"))
                (:freeze-access-level :boolean ,(s-prefix "ext:freezeAccessLevel"))) ;; deprecated
  :has-one `((case                    :via ,(s-prefix "dct:hasPart")
                                      :inverse t
                                      :as "case")
             (meeting                 :via ,(s-prefix "besluitvorming:isAangevraagdVoor")
                                      :as "requested-for-meeting")
             (access-level            :via ,(s-prefix "ext:toegangsniveauVoorProcedurestap")
                                      :as "access-level")
             (subcase-type            :via ,(s-prefix "dct:type")
                                      :as "type")
             (newsletter-info         :via ,(s-prefix "prov:generated")
                                      :as "newsletter-info")
             (mandatee                :via ,(s-prefix "ext:indiener")
                                      :as "requested-by")
             (user                    :via      ,(s-prefix "ext:modifiedBy")
                                      :as "modified-by"))
  :has-many `((person                 :via ,(s-prefix "dct:creator") ;; heeftCreator?  ;; NOTE: used persoon instead of agent
                                      :as "heeftCreator")
              (mandatee               :via ,(s-prefix "besluitvorming:heeftBevoegde") ;; NOTE: used mandataris instead of agent
                                      :as "mandatees")
              (subcase                :via ,(s-prefix "dct:relation")
                                      :as "related-to")
              (document               :via ,(s-prefix "ext:bevatDocumentversie") ;; NOTE: instead of dct:hasPart (mu-cl-resources relation type checking workaround)
                                      :as "document-versions")
              (document               :via ,(s-prefix "ext:bevatReedsBezorgdeDocumentversie") ;; NOTE: instead of dct:hasPart (mu-cl-resources relation type checking workaround)
                                      :as "linked-document-versions")
              (agendaitem             :via ,(s-prefix "besluitvorming:isGeagendeerdVia")
                                      :as "agendaitems")
              (subcase-phase          :via ,(s-prefix "ext:subcaseProcedurestapFase")
                                      :as "phases")
              (remark                 :via ,(s-prefix "besluitvorming:opmerking")
                                      :as "remarks")
              (ise-code               :via ,(s-prefix "ext:heeftInhoudelijkeStructuurElementen")
                                      :as "ise-codes")
              (decision               :via ,(s-prefix "ext:procedurestapHeeftBesluit") ;; NOTE: instead of dct:hasPart (mu-cl-resources relation type checking workaround)
                                      :as "decisions"))
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

(define-resource subcase-phase () ;; NOTE: Should be subclass of besluitvorming:Status (mu-cl-resources reasoner workaround)
  :class (s-prefix "ext:ProcedurestapFase") ;; NOTE: as well as skos:Concept
  :properties `((:remark :string ,(s-prefix "rdfs:comment"))
                (:label :string ,(s-prefix "skos:prefLabel"))
                (:date :datetime ,(s-prefix "besluitvorming:statusdatum")))
  :has-one `((subcase             :via ,(s-prefix "ext:subcaseProcedurestapFase")
                                  :inverse t
                                  :as "subcase")
             (agendaitem          :via ,(s-prefix "ext:subcaseAgendapuntFase")
                                  :inverse t
                                  :as "agendaitem")
             (subcase-phase-code  :via ,(s-prefix "ext:procedurestapFaseCode")
                                  :as "code"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/procedurestap-fases/")
  :features '(include-uri)
  :on-path "subcase-phases")

(define-resource subcase-phase-code ()
  :class (s-prefix "ext:ProcedurestapFaseCode")
  :properties `((:label :string ,(s-prefix "skos:prefLabel"))
                (:scope-note :string ,(s-prefix "skos:scopeNote"))
                (:alt-label :string ,(s-prefix "skos:altLabel")))
  :has-many `((subcase-phase :via ,(s-prefix "ext:procedurestapFaseCode")
                          :inverse t
                          :as "subcase-phases")
              (subcase-phase-code :via ,(s-prefix "skos:broader") ;; NOTE: tree structure for type-hierarchy (cfr codelist)
                                        :inverse t
                                        :as "subphase-codes"))
  :has-one `((subcase-phase-code :via ,(s-prefix "skos:broader")
                                      :as "superphase"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/concept/procedurestap-fase-codes/")
  :features '(include-uri)
  :on-path "subcase-phase-codes")

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
