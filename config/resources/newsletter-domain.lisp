(define-resource newsletter-info ()
  :class (s-prefix "besluitvorming:NieuwsbriefInfo")
  :properties `((:text                  :string   ,(s-prefix "besluitvorming:inhoud"))
                (:richtext              :string   ,(s-prefix "ext:htmlInhoud"))
                (:subtitle              :string   ,(s-prefix "dbpedia:subtitle"))
                (:publication-date      :datetime ,(s-prefix "dct:issued"))
                (:publication-doc-date  :datetime ,(s-prefix "ext:issuedDocDate"))
                (:mandatee-proposal     :string   ,(s-prefix "ext:voorstelVan"))
                (:title                 :string   ,(s-prefix "dct:title"))
                (:finished              :boolean  ,(s-prefix "ext:afgewerkt"))
                (:in-newsletter         :boolean  ,(s-prefix "ext:inNieuwsbrief"))
                (:remark                :string   ,(s-prefix "ext:opmerking"))
                (:modified              :datetime ,(s-prefix "ext:aangepastOp")))
  :has-one `((meeting                   :via      ,(s-prefix "ext:algemeneNieuwsbrief")
                                        :inverse t
                                        :as "meeting")
             (agenda-item-treatment     :via      ,(s-prefix "prov:generated")
                                        :inverse t
                                        :as "agenda-item-treatment")
             (user                      :via      ,(s-prefix "ext:modifiedBy")
                                        :as "modified-by"))
  :has-many `((theme                    :via      ,(s-prefix "dct:subject")
                                        :as "themes")
              (piece                    :via      ,(s-prefix "ext:documentenVoorPublicatie")
                                        :as "pieces")
              )
  :resource-base (s-url "http://themis.vlaanderen.be/id/nieuwsbrief-info/")
  :features '(include-uri)
  :on-path "newsletter-infos")

(define-resource theme ()
  :class (s-prefix "ext:ThemaCode") ;; NOTE: as well as skos:Concept
  :properties `((:label         :string ,(s-prefix "skos:prefLabel"))
                (:scope-note    :string ,(s-prefix "skos:scopeNote"))
                (:alt-label     :string ,(s-prefix "skos:altLabel"))
                (:deprecated    :bool   ,(s-prefix "owl:deprecated")))
  :has-many `((newsletter-info  :via    ,(s-prefix "dct:subject")
                                :inverse t
                                :as "newsletters"))
  :resource-base (s-url "http://themis.vlaanderen.be/id/concept/thema/")
  :features `(no-pagination-defaults include-uri)
  :on-path "themes")

(define-resource mail-campaign ()
  :class (s-prefix "ext:MailCampagne")
  :properties `((:campaign-id       :string   ,(s-prefix "ext:campagneId"))
                (:campaign-web-id   :string   ,(s-prefix "ext:campagneWebId"))
                (:archive-url       :string   ,(s-prefix "ext:voorbeeldUrl"))
                (:sent-at           :datetime ,(s-prefix "ext:isVerstuurdOp")))
  :has-one `((meeting               :via      ,(s-prefix "ext:heeftMailCampagnes")
                                    :inverse t
                                    :as "meeting"))
  :resource-base (s-url "http://themis.vlaanderen.be/id/mailcampagne/")
  :features `(include-uri)
  :on-path "mail-campaigns")

(define-resource internal-decision-publication-activity () ; FIXME extend prov:Activity
  :class (s-prefix "ext:InternalDecisionPublicationActivity")
  :properties `((:start-time       :datetime     ,(s-prefix "prov:startedAtTime"))
              )
  :has-one `((meeting              :via          ,(s-prefix "ext:internalDecisionPublicationActivityUsed") ; FIXME prov:used / workaround for meeting having multiple relationships of a subtype of prov:Activity
                                   :as "meeting"))
  :resource-base (s-url "http://themis.vlaanderen.be/id/internebeslissingspublicatieactiviteit/")
  :features `(include-uri)
  :on-path "internal-decision-publication-activities")

; "Vrijgave" release of documents within the government
(define-resource internal-document-publication-activity () ; FIXME extend prov:Activity
  :class (s-prefix "ext:InternalDocumentPublicationActivity")
  :properties `((:start-time       :datetime     ,(s-prefix "prov:startedAtTime")) ; time the publication process is started
                (:planned-publication-time           :datetime   ,(s-prefix "generiek:geplandeStart")) ; time to set the start-date that should
                (:unconfirmed-publication-time       :datetime   ,(s-prefix "ext:onbevestigdePublicatietijd"))
              )
  :has-one `((meeting              :via          ,(s-prefix "ext:internalDocumentPublicationActivityUsed") ; FIXME prov:used / workaround for meeting having multiple relationships of a subtype of prov:Activity
                                   :as "meeting"))
  :resource-base (s-url "http://themis.vlaanderen.be/id/internedocumentpublicatieactiviteit/")
  :features `(include-uri)
  :on-path "internal-document-publication-activities")

(define-resource themis-publication-activity ()
  :class (s-prefix "ext:ThemisPublicationActivity")
  :properties `((:start-time       :datetime     ,(s-prefix "prov:startedAtTime"))
                  ; time the publication process has started
                  ; optional ; not set when not yet started
                (:planned-publication-time          :datetime     ,(s-prefix "generiek:geplandeStart")) ; time the publication process should end
                (:unconfirmed-publication-time      :datetime     ,(s-prefix "ext:onbevestigdePublicatietijd")) ; time to set the start-date that is to be confirmed
                (:scope            :string-set   ,(s-prefix "ext:scope"))
              )
  :has-one `((meeting              :via          ,(s-prefix "prov:used")
                                   :as "meeting"))
  :resource-base (s-url "http://themis.vlaanderen.be/id/themis-publicatie-activiteit/")
  :features `(include-uri)
  :on-path "themis-publication-activities")
