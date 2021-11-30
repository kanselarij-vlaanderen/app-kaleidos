
(define-resource mandatee ()
  :class (s-prefix "mandaat:Mandataris")
  :properties `((:priority        :number ,(s-prefix "mandaat:rangorde"))
                (:start           :datetime ,(s-prefix "mandaat:start"))
                (:end             :datetime ,(s-prefix "mandaat:einde"))
                (:newsletter-title :string ,(s-prefix "ext:nieuwsbriefTitel")) ; As opposed to the dct:title, this property includes the name of the mandatee
                (:title           :string ,(s-prefix "dct:title")))
  :has-many `((approval            :via ,(s-prefix "ext:goedkeuringen")
                                  :as "approvals")
             (subcase             :via ,(s-prefix "ext:heeftBevoegde")
                                  :inverse t
                                  :as "subcases")
             (publication-flow    :via ,(s-prefix "ext:heeftBevoegdeVoorPublicatie")
                                  :inverse t
                                  :as "publication-flows")
             (agendaitem          :via ,(s-prefix "ext:heeftBevoegdeVoorAgendapunt")
                                  :inverse t
                                  :as "agendaitems")
             (subcase             :via ,(s-prefix "ext:indiener")
                                  :inverse t
                                  :as "requested-subcases")
             (sign-signing-activity :via ,(s-prefix "sign:ondertekenaar")
                                  :inverse t
                                  :as "sign-signing-activities")
             )
  :has-one `((person              :via ,(s-prefix "mandaat:isBestuurlijkeAliasVan")
                                  :as "person")
             (mandate             :via ,(s-prefix "org:holds")
                                  :as "mandate")
             (government-body     :via ,(s-prefix "prov:hadMember")
                                  :inverse t
                                  :as "government-body"))
  :resource-base (s-url "http://themis.vlaanderen.be/id/mandataris/")
  :features '(include-uri)
  :on-path "mandatees")

(define-resource mandate ()
  :class (s-prefix "mandaat:Mandaat")
  :has-one `((role              :via ,(s-prefix "org:role")
                                :as "role"))
  :resource-base (s-url "http://themis.vlaanderen.be/id/mandaat/")
  :features '(include-uri)
  :on-path "mandates")

(define-resource role ()
  :class (s-prefix "org:Role")
  :properties `((:label        :string ,(s-prefix "skos:prefLabel")))
  :resource-base (s-url "http://themis.vlaanderen.be/id/bestuursfunctie/")
  :features '(include-uri)
  :on-path "roles")

(define-resource person ()
  :class (s-prefix "person:Person")
  :properties `((:last-name         :string ,(s-prefix "foaf:familyName"))
                (:first-name        :string ,(s-prefix "persoon:gebruikteVoornaam"))
              )
  :has-many `((mandatee             :via    ,(s-prefix "mandaat:isBestuurlijkeAliasVan")
                                    :inverse t
                                    :as "mandatees"))
  :has-one `((identification        :via    ,(s-prefix "ext:identifier")
                                    :as "identifier")
             (contact-person        :via ,(s-prefix "schema:contactPoint")
                                    :as "contact-person")
             (organization          :via ,(s-prefix "org:hasMember")
                                    :inverse t
                                    :as "organization")
            )
  :resource-base (s-url "http://themis.vlaanderen.be/id/persoon/")
  :features '(include-uri)
  :on-path "persons")

(define-resource government-body ()
  :class (s-prefix "besluit:Bestuursorgaan")
  :properties `((:name        :string ,(s-prefix "skos:prefLabel")))
  :has-one `((mandatee        :via ,(s-prefix "prov:hadMember")
                              :as "government-body"))
  :resource-base (s-url "http://themis.vlaanderen.be/id/bestuursorgaan/")
  :features '(include-uri)
  :on-path "government-bodies")

