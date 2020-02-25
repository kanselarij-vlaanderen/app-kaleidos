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
              (document-container :via ,(s-prefix "dct:hasPart")
                        :as "documents"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/consultatie-antwoorden/")
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

(define-resource translation-request ()
  :class (s-prefix "besluitvorming:Vertalingsaanvraag")
  :properties `((:target-language :string ,(s-prefix "besluitvorming:doeltaal"))
                (:expected-delivery-date :date ,(s-prefix "besluitvorming:verwachteOpleverdatum")))
  :has-many `(
              (document         :via ,(s-prefix "prov:generated")
                                :as "documentVersions")
              (translation-state :via ,(s-prefix "ext:vertalingsaanvraagStatus")
                                  :as "states")
              (remark :via ,(s-prefix "besluitvorming:opmerking")
                      :as "remarks")) ;; NOTE: opmerkingEN would be more suitable?
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/vertalingsaanvragen/")
  :features '(include-uri)
  :on-path "translation-requests")

(define-resource translation-state ()
  :class (s-prefix "besluitvorming:VertalingsaanvraagStatus") ;; NOTE: Should be subclass of besluitvorming:Status (mu-cl-resources reasoner workaround)
  :properties `((:date :datetime ,(s-prefix "besluitvorming:statusdatum")))
  :has-many `((remark :via ,(s-prefix "rdfs:comment")
                      :as "remarks"))
  :has-one `((translation-request :via ,(s-prefix "ext:vertalingsaanvraagStatus") ;; NOTE: More specific relationship then besluitvorming:status as mu-cl-resources workaround
                                 :inverse t
                                 :as "translationRequest")
             (translation-state-name :via ,(s-prefix "ext:vertalingsaanvraagStatusCode")
                                     :inverse t
                                     :as "value"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/vertalingsaanvraag-statussen/")
  :features '(include-uri)
  :on-path "translation-states")

(define-resource translation-state-name ()
  :class (s-prefix "besluitvorming:VertalingsaanvraagStatusCode") ;; NOTE: Should be subclass of besluitvorming:Status (mu-cl-resources reasoner workaround)
  :properties `((:label :string ,(s-prefix "skos:prefLabel"))
                (:alt-label :string ,(s-prefix "skos:altLabel")))
  :has-many `((translation-state :via ,(s-prefix "ext:vertalingsaanvraagStatusCode")
                                 :inverse t
                                 :as "translation-states"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/concept/vertalingsaanvraag-status-codes/")
  :features '(include-uri)
  :on-path "translation-state-code")
