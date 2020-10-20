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

;; (define-resource publication ()
;;   :class (s-prefix "ext:Publicatie")
;;   :properties `((:       :string   ,(s-prefix "ext:*"))
;;                 (:title                 :string   ,(s-prefix "dct:title"))
;;                 (:short-title         :string   ,(s-prefix "dct:alternative"))
;;                 (:*       :string   ,(s-prefix "ext:*"))
;;                 (:*           :datetime ,(s-prefix "ext:*")))

;;   :has-one `((case                  :via      ,(s-prefix "ext:publicatieVan") ;; gaan we bestaande case gebruiken hiervoor? is niet voor kanselarij
;;                                     :as "case"))
;;   :has-many `((translation          :via      ,(s-prefix "ext:vertaling")
;;                                     :as "translations"))
;;   :resource-base (s-url "http://kanselarij.vo.data.gift/id/publicatie/")
;;   :features `(no-pagination-defaults include-uri)
;;   :on-path "publicaties")

  ;; KaS-1868 historiek ? momenteel weg gehaald, mogelijk later terug
  ;; aangevraagd door, instantie, heeft ook besluitdomeinen
  ;; intern dossiernummer (publicatienummer) in publicatie of in dossier ? Hebben VR dossiers een ander nummering systeem ?
  ;; nota, richtText of is dit korte en lange titel?
  ;; status publicatie
  ;; uiterste vertaaldatum
  ;; uiterste datum publicatie
  ;; wijze van publicate (type)
  ;; opmerking (persoon wie opmerking gemaakt heeft lijkt niet nodig)
  ;; documenten (pdf en word aan elkaar kunnen hangen), opsplitsen ?   // moet vertaald worden bool, moet in drukproef bool, type, geupload op
  ;; vertalingen krijgen fysiek een bestand, model maken paralell op serie? meerdere pieces die gelinkt zijn aan 1 iets, alle files zijn ofwel andere extensie of vertaald.
  ;; verstuurd naar BS ?
  ;; handtekeningen gebeuren op de PDF, vertalingen op de PDF
  ;; werknummer BS
  ;; publicatie datum in BS
  ;; BS status apart has One?



  
