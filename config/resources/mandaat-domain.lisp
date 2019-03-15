(define-resource birth ()
  :class (s-prefix "persoon:Geboorte")
  :properties `((:date :date ,(s-prefix "persoon:datum")))
  :resource-base (s-url "http://data.lblod.info/id/geboortes/")
  :features '(include-uri)
  :on-path "geboortes")

(define-resource mandate ()
  :class (s-prefix "mandaat:Mandaat")
  :properties `((:number-of-mandatees :number ,(s-prefix "mandaat:aantalHouders")))
  :has-one `((government-function :via ,(s-prefix "org:role")
                                  :as "function")
             (government-body :via ,(s-prefix "org:hasPost")
                                   :inverse t
                                   :as "bevat-in"))
  :resource-base (s-url "http://data.lblod.info/id/mandaten/")
  :features '(include-uri)
  :on-path "mandates")

;; TODO:karel why do we need this?
(define-resource government-function ()
  :class (s-prefix "ext:BestuursfunctieCode")
  :properties `((:label :string ,(s-prefix "skos:prefLabel"))
                (:scope-note :string ,(s-prefix "skos:scopeNote")))
  :resource-base (s-url "http://data.vlaanderen.be/id/concept/BestuursfunctieCode/")
  :features '(include-uri)
  :on-path "government-functions")

(define-resource mandatee ()
  :class (s-prefix "mandaat:Mandataris")
  :properties `((:priority :string ,(s-prefix "mandaat:rangorde"))
                (:start :datetime ,(s-prefix "mandaat:start"))
                (:end :datetime ,(s-prefix "mandaat:einde"))
                (:date-sworn-in :datetime ,(s-prefix "ext:datumEedaflegging"))
                (:date-decree :datetime ,(s-prefix "ext:datumMinistrieelBesluit"))
                (:title :string ,(s-prefix "dct:title")))
  :has-many `(
    ;; (mandatee :via ,(s-prefix "mandaat:isTijdelijkVervangenDoor")
    ;;                     :as "temporary-replacements")
              (government-domain :via ,(s-prefix "mandaat:beleidsdomein")
                                 :as "government-domains")
              (decision :via ,(s-prefix "besluitvorming:neemtBesluit") ;; NOTE: What is the URI of property 'neemt' (Agent neemt besluit)? Guessed besluitvorming:neemtBesluit 
                        :inverse t
                        :as "decisions")
              (case     :via ,(s-prefix "besluitvorming:heeftBevoegde") ;; NOTE: used mandataris instead of agent
                        :inverse t
                        :as "cases")
              (meeting-record :via ,(s-prefix "ext:aanwezigen")
                              :as "meetings-attended"))
  :has-one `((mandate   :via ,(s-prefix "org:holds")
                        :as "holds")
             (person    :via ,(s-prefix "mandaat:isBestuurlijkeAliasVan")
                        :as "person")
             (mandatee-state :via ,(s-prefix "mandaat:status")
                              :as "state"))
  :resource-base (s-url "http://data.lblod.info/id/mandatarissen/")
  :features '(include-uri)
  :on-path "mandatees")

(define-resource mandatee-state ()
  :class (s-prefix "ext:MandatarisStatusCode")
  :properties `((:label :string ,(s-prefix "skos:prefLabel"))
                (:scope-note :string ,(s-prefix "skos:scopeNote")))
  :resource-base (s-url "http://data.vlaanderen.be/id/concept/MandatarisStatusCode/")
  :features '(include-uri)
  :on-path "madatee-states")

(define-resource government-domain ()
  :class (s-prefix "ext:BeleidsdomeinCode")
  :properties `((:label :string ,(s-prefix "skos:prefLabel"))
                (:scope-note :string ,(s-prefix "skos:scopeNote")))
  :has-many `((mandatee :via ,(s-prefix "mandaat:beleidsdomein")
                        :inverse t
                        :as "mandatees"))
  :resource-base (s-url "http://data.vlaanderen.be/id/concept/BeleidsdomeinCode/")
  :features '(include-uri)
  :on-path "government-domains")

(define-resource jurisdiction ()
  :class (s-prefix "ext:BevoegdheidCode")
  :properties `((:label :string ,(s-prefix "skos:prefLabel"))
                (:scope-note :string ,(s-prefix "skos:scopeNote")))
  :has-many `((mandatee :via ,(s-prefix "ext:bevoegdheid")
                        :inverse t
                        :as "mandatees"))
  :resource-base (s-url "http://data.vlaanderen.be/id/concept/BevoegdheidCode/")
  :features '(include-uri)
  :on-path "responsibilities")

(define-resource person ()
  :class (s-prefix "person:Person")
  :properties `((:last-name         :string ,(s-prefix "foaf:familyName"))
                (:alternative-name  :string ,(s-prefix "foaf:name"))
                (:first-name        :string ,(s-prefix "foaf:firstName")))
  :has-many `((mandatee             :via    ,(s-prefix "mandaat:isBestuurlijkeAliasVan")
                                    :inverse t
                                    :as "mandatees"))
  :has-one `((identification        :via    ,(s-prefix "ext:identifier")
                                    :as "identifier"))
             ;; (gender :via ,(s-prefix "persoon:geslacht")
             ;;         :as "gender")
             ;; (birth :via ,(s-prefix "persoon:heeftGeboorte")
             ;;      :as "birth")
  :resource-base (s-url "http://data.lblod.info/id/personen/")
  :features '(include-uri)
  :on-path "people")

(define-resource gender ()
  :class (s-prefix "ext:GeslachtCode")
  :properties `((:label :string ,(s-prefix "skos:prefLabel"))
                (:scope-note :string ,(s-prefix "skos:scopeNote")))
  :resource-base (s-url "http://data.vlaanderen.be/id/concept/GeslachtCode/")
  :features '(include-uri)
  :on-path "genders")

(define-resource identification ()
  :class (s-prefix "adms:Identifier")
  :properties `((:id-name :string ,(s-prefix "skos:notation"))) ;; TODO: should have a specific type
  :has-one `((person :via ,(s-prefix "ext:identifier")
                     :inverse t
                     :as "personId"))
  :resource-base (s-url "http://data.lblod.info/id/identificatoren/")
  :features '(include-uri)
  :on-path "identifications")
