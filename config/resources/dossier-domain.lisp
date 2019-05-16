(define-resource case ()
  :class (s-prefix "dbpedia:Case")
  :properties `((:created       :datetime ,(s-prefix "dct:created")) ;; NOTE: Type should be :date instead?
                (:short-title   :string   ,(s-prefix "dct:alternative"))
                (:number        :number   ,(s-prefix "adms:identifier")) ;; NOTE: Type should be :number instead?
                (:is-archived   :boolean   ,(s-prefix "ext:isGearchiveerd"))
                (:title         :string   ,(s-prefix "dct:title"))
                (:confidential  :boolean  ,(s-prefix "ext:vertrouwelijk"))
                (:policy-level  :string   ,(s-prefix "ext:beleidsNiveau")))
  :has-one `((confidentiality   :via ,(s-prefix "besluitvorming:vertrouwelijkheid")
                                :as "confidentiality")
             (case-type         :via      ,(s-prefix "dct:type")
                                :as "type"))
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
  :properties `((:label :string ,(s-prefix "skos:prefLabel"))
                (:scope-note :string ,(s-prefix "skos:scopeNote"))
                (:alt-label :string ,(s-prefix "skos:altLabel")))
  :has-many `((case :via ,(s-prefix "dct:type")
                       :inverse t
                       :as "cases"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/concept/dossier-type-codes/")
  :features '(include-uri)
  :on-path "case-types")


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
                (:confidential           :boolean ,(s-prefix "besluitvorming:besloten"))
                (:show-as-remark      :boolean ,(s-prefix "ext:wordtGetoondAlsMededeling"))) ;; NOTE: supplementary addition to model
  :has-one `((decision                :via ,(s-prefix "ext:procedurestapHeeftBesluit") ;; NOTE: instead of dct:hasPart (mu-cl-resources relation type checking workaround)
                                      :as "decision")
             (case                    :via ,(s-prefix "dct:hasPart")
                                      :inverse t
                                      :as "case")
             (meeting                 :via ,(s-prefix "besluitvorming:isAangevraagdVoor")
                                      :as "requested-for-meeting")
             (confidentiality         :via ,(s-prefix "besluitvorming:vertrouwelijkheid")
                                      :as "confidentiality")
             (subcase-type            :via ,(s-prefix "dct:type")
                                      :as "type"))
  :has-many `((approval               :via      ,(s-prefix "ext:procedurestapGoedkeuring")
                                      :as "approvals")
              (theme                  :via ,(s-prefix "dct:subject")
                                      :as "themes")
              (person                 :via ,(s-prefix "dct:creator") ;; heeftCreator?  ;; NOTE: used persoon instead of agent
                                      :as "heeftCreator")
              (mandatee               :via ,(s-prefix "besluitvorming:heeftBevoegde") ;; NOTE: used mandataris instead of agent
                                      :as "mandatees")
              (subcase                :via ,(s-prefix "dct:relation")
                                      :as "related-to")
              (document-version       :via ,(s-prefix "ext:bevatDocumentversie") ;; NOTE: instead of dct:hasPart (mu-cl-resources relation type checking workaround)
                                      :as "document-versions")
              (consultation-request   :via ,(s-prefix "ext:bevatConsultatievraag") ;; NOTE: instead of dct:hasPart (mu-cl-resources relation type checking workaround)
                                      :as "consultationRequests") ;; NOTE: consultatieVRAGEN would be more suitable?
              (agendaitem             :via ,(s-prefix "besluitvorming:isGeagendeerdVia")
                                      :as "agendaitems")
              (subcase-phase          :via ,(s-prefix "ext:subcaseProcedurestapFase")
                                      :as "phases")
              (remark                 :via ,(s-prefix "besluitvorming:opmerking")
                                      :as "remarks")
              (ise-code               :via ,(s-prefix "ext:heeftInhoudelijkeStructuurElementen")
                                      :as "ise-codes"))
  :resource-base (s-url "http://data.vlaanderen.be/id/Procedurestap/")
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
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/concept/procedurestap-type/")
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

(define-resource confidentiality ()
  :class (s-prefix "ext:VertrouwelijkheidCode") ;; NOTE: as well as skos:Concept
  :properties `((:label       :string ,(s-prefix "skos:prefLabel"))
                (:scope-note  :string ,(s-prefix "skos:scopeNote"))
                (:alt-label :string ,(s-prefix "skos:altLabel")))
  :has-many `((subcase        :via ,(s-prefix "besluitvorming:vertrouwelijkheid")
                              :inverse t
                              :as "subcases")
              (document       :via ,(s-prefix "besluitvorming:vertrouwelijkheid")
                              :inverse t
                              :as "documents")
              (agendaitem      :via ,(s-prefix "besluitvorming:vertrouwelijkheidAgendapunt")
                              :inverse t
                              :as "agendaitems"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/concept/vertrouwelijkheid-codes/")
  :features '(include-uri)
  :on-path "confidentialities")

(define-resource person-or-organization ()
  :class (s-prefix "ext:PersonOrOrganization") ;; NOTE: as resource hack for super typing, is person or organization
  :properties `((:type :string ,(s-prefix "rdfs:type")))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/personen/") ;; NOTE: Should in theory never get used, as this is a read-only hack.
  :features '(include-uri)
  :on-path "person-or-organization")

(define-resource consultation-request ()
  :class (s-prefix "besluitvorming:Consultatievraag")
  :properties `((:request :string ,(s-prefix "besluitvorming:aanvraag"))
                (:date :datetime ,(s-prefix "besluitvorming:aanvraagdatum"))) ;; NOTE: Type should be :date instead?
  :has-one `((subcase :via ,(s-prefix "ext:bevatConsultatievraag")
                            :inverse t
                            :as "subcase")
             (consultation-type :via ,(s-prefix "dct:type")
                               :as "type")
             (person-or-organization :via ,(s-prefix "besluitvorming:isGesteldAan") ;; NOTE: shoudl be Agent?
                                     :as "isGesteldAan")
             (person :via ,(s-prefix "besluitvorming:heeftContactpersoon") ;; NOTE: used persoon instead of agent
                     :as "contactPerson")
             (consultation-response :via ,(s-prefix "prov:generated")
                                    :as "response"))
  :has-many `((remark :via ,(s-prefix "besluitvorming:opmerking") ;; NOTE: opmerkingEN would be more suitable?
                      :as "remarks"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/consultatievragen/")
  :features '(include-uri)
  :on-path "consultation-requests")

(define-resource consultation-type ()
  :class (s-prefix "besluitvorming:Consultatietype")
  :properties `((:label :string ,(s-prefix "skos:prefLabel"))
                (:scope-note :string ,(s-prefix "skos:scopeNote"))
                (:alt-label :string ,(s-prefix "skos:altLabel")))
  :has-many `((consultation-request :via ,(s-prefix "dct:type")
                          :inverse t
                          :as "requests"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/concept/consultatie-type-codes/")
  :features '(include-uri)
  :on-path "consultation-types")

(define-resource consultation-response ()
  :class (s-prefix "besluitvorming:Consultatie-antwoord")
  :properties `((:date :datetime ,(s-prefix "besluitvorming:ontvangstdatum")) ;; NOTE: Type should be :date instead?
                (:text :string ,(s-prefix "besluitvorming:samenvatting")))
  :has-one `((consultation-response-code :via ,(s-prefix "besluitvorming:uitkomst")
                                         :as "result")
             (consultation-request :via ,(s-prefix "prov:generated")
                                   :inverse t
                                   :as "consultation-request"))
  :has-many `((remark :via ,(s-prefix "besluitvorming:opmerking")
                      :as "remarks")
              (document :via ,(s-prefix "dct:hasPart")
                        :as "documents"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/consultatievragen/")
  :features '(include-uri)
  :on-path "consultation-responses")

(define-resource consultation-response-code ()
  :class (s-prefix "ext:Consultatie-uitkomstCode")
  :properties `((:label :string ,(s-prefix "skos:prefLabel"))
                (:scope-note :string ,(s-prefix "skos:scopeNote"))
                (:alt-label :string ,(s-prefix "skos:altLabel")))
  :has-many `((consultation-response :via ,(s-prefix "besluitvorming:uitkomst")
                                     :inverse t
                                     :as "consultation-responses"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/concept/consultatie-uitkomst-codes/")
  :features '(include-uri)
  :on-path "consultation-response-codes")

