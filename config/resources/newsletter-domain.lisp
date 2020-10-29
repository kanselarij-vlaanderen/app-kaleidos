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
  :has-one `((agenda-item-treatment     :via      ,(s-prefix "prov:generated")
                                        :inverse t
                                        :as "agenda-item-treatment")
             (meeting                   :via      ,(s-prefix "ext:algemeneNieuwsbrief")
                                        :inverse t
                                        :as "meeting")
             (user                      :via      ,(s-prefix "ext:modifiedBy")
                                        :as "modified-by"))
  :has-many `((theme                    :via      ,(s-prefix "dct:subject")
                                        :as "themes")
              (piece                    :via      ,(s-prefix "ext:documentenVoorPublicatie")
                                        :as "pieces"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/nieuwsbrief-infos/")
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
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/concept/thema-codes/")
  :features `(no-pagination-defaults include-uri)
  :on-path "themes")

(define-resource mail-campaign ()
  :class (s-prefix "ext:MailCampagne")
  :properties `((:campaign-id       :string   ,(s-prefix "ext:campagneId"))
                (:campaign-web-id   :string   ,(s-prefix "ext:campagneWebId"))
                (:archive-url       :string   ,(s-prefix "ext:voorbeeldUrl"))
                (:sent-at           :datetime ,(s-prefix "ext:isVerstuurdOp")))
  :has-many `((meeting              :via      ,(s-prefix "ext:heeftMailCampagnes")
                                    :inverse t
                                    :as "meetings"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/concept/mail-campaigns/")
  :features `(no-pagination-defaults include-uri)
  :on-path "mail-campaigns")
