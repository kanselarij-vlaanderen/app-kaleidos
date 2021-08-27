
(define-resource mandatee ()
  :class (s-prefix "mandaat:Mandataris")
  :properties `((:priority        :number ,(s-prefix "mandaat:rangorde"))
                (:start           :datetime ,(s-prefix "mandaat:start"))
                (:end             :datetime ,(s-prefix "mandaat:einde"))
                (:date-sworn-in   :datetime ,(s-prefix "ext:datumEedaflegging"))
                (:date-decree     :datetime ,(s-prefix "ext:datumMinistrieelBesluit"))
                (:nick-name       :string ,(s-prefix "ext:nickName")) ;; Contains role of mandatee (MP, VMP, Minister)
                (:title           :string ,(s-prefix "dct:title")))
  :has-many `((ise-code            :via ,(s-prefix "ext:heeftBevoegdeMandataris")
                                  :as "ise-codes")
             (approval            :via ,(s-prefix "ext:goedkeuringen")
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
             (sign-activity       :via ,(s-prefix "sign:ondertekenaar")
                                  :inverse t
                                  :as "sign-activities")
             )
  :has-one `((person              :via ,(s-prefix "mandaat:isBestuurlijkeAliasVan")
                                  :as "person"))
  :resource-base (s-url "http://themis.vlaanderen.be/id/mandataris/")
  :features '(include-uri)
  :on-path "mandatees")

(define-resource person ()
  :class (s-prefix "person:Person")
  :properties `((:last-name         :string ,(s-prefix "foaf:familyName"))
                (:alternative-name  :string ,(s-prefix "foaf:name"))
                (:first-name        :string ,(s-prefix "foaf:firstName"))
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
             (sign-flow             :via ,(s-prefix "dct:creator")
                                    :inverse t
                                    :as "sign-flow")
            )
  :resource-base (s-url "http://themis.vlaanderen.be/id/persoon/")
  :features '(include-uri)
  :on-path "persons")
