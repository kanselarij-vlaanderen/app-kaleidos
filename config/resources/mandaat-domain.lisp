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
                                  :as "requested-subcases"))
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
                (:email-link        :url    ,(s-prefix "foaf:mbox"))
                (:phone-link        :url    ,(s-prefix "foaf:phone")))
  :has-many `((mandatee             :via    ,(s-prefix "mandaat:isBestuurlijkeAliasVan")
                                    :inverse t
                                    :as "mandatees"))
  :has-one `((identification        :via    ,(s-prefix "ext:identifier")
                                    :as "identifier")
             (signature             :via    ,(s-prefix "ext:bevoegdePersoon")
                                    :inverse t
                                    :as "signature"))
  :resource-base (s-url "http://themis.vlaanderen.be/id/persoon/")
  :features '(include-uri)
  :on-path "persons")
  ;; People here because ember pluralizes persons to people

  (define-resource signature ()
  :class (s-prefix "ext:Handtekening")
  :properties `((:name      :string     ,(s-prefix "ext:zichtbareNaam"))
                (:function  :string     ,(s-prefix "ext:functie"))
                (:is-active :boolean    ,(s-prefix "ext:isStandaard"))
                (:to-delete :string     ,(s-prefix "ext:toDelete")))
  :has-one `((person        :via        ,(s-prefix "ext:bevoegdePersoon")
                            :as "person")
             (file          :via        ,(s-prefix "ext:handtekening")
                            :as "file"))
  :has-many `((meeting      :via       ,(s-prefix "ext:heeftHandtekening")
                            :inverse t
                            :as "meetings"))
  :resource-base (s-url "http://themis.vlaanderen.be/id/handtekening/")
  :features '(include-uri)
  :on-path "signatures")

;; Contact person is on purpose a separate model.In the end this is supposed to be a PERSON
;; but for now they are entering a lot of duplicate data and we dont want to pullute the  persons model with this metadata.
;; same for organisation. This is a string metadata field for publication flow.
;; In the future you want to link an exiusting contact from an existing organisation and this model should  ont exist anylonger.

(define-resource contact-person ()
  :class (s-prefix "pub:ContactPersoon")
  :properties `((:last-name         :string ,(s-prefix "foaf:familyName"))
                (:alternative-name  :string ,(s-prefix "foaf:name"))
                (:first-name        :string ,(s-prefix "foaf:firstName"))
                (:email             :string    ,(s-prefix "foaf:mbox"))
                (:organisation-name :string ,(s-prefix "pub:organisationName"))
                (:phone             :string    ,(s-prefix "foaf:phone")))
  :has-one `((publication-flow      :via      ,(s-prefix "pub:contactPersoon")
                                    :inverse t
                                    :as "publication-flow")
            (organization :via ,(s-prefix "org:memberOf")
                                    :as "organization"))
  :resource-base (s-url "http://themis.vlaanderen.be/id/contactpersoon/")
  :features '(include-uri)
  :on-path "contact-persons")
