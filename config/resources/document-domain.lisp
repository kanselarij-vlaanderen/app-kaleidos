(define-resource document-container ()
  :class (s-prefix "dossier:Serie")
  :properties `((:created               :datetime ,(s-prefix "dct:created")))
  :has-many `((piece                    :via ,(s-prefix "dossier:collectie.bestaatUit") ;; TODO should become `dossier:Collectie.bestaatUit`
                                        :as "pieces"))
  :has-one `((document-type             :via ,(s-prefix "ext:documentType")
                                        :as "type")
             (meeting-record            :via ,(s-prefix "ext:getekendeNotulen")
                                        :inverse t
                                        :as "signed-minutes")
                                        )
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/series/")
  :features '(include-uri)
  :on-path "document-containers")

;; Generieke “serie” wordt gebruikt voor specifieke doeleinden voor het groeperen van stukken (KAS-1558)
(define-resource piece ()
  :class (s-prefix "dossier:Stuk")
  :properties `((:name                  :string   ,(s-prefix "dct:title"))
                (:created               :datetime ,(s-prefix "dct:created"))
                (:modified              :datetime ,(s-prefix "dct:modified"))
                (:confidential          :boolean  ,(s-prefix "ext:vertrouwelijk"))
                (:access-level-last-modified          :datetime  ,(s-prefix "ext:accessLevelLastModified")))
  :has-one `((access-level              :via ,(s-prefix "ext:toegangsniveauVoorDocumentVersie")
                                        :as "access-level")
            (file                       :via      ,(s-prefix "ext:file")
                                        :as "file")
            (file                       :via      ,(s-prefix "ext:convertedFile")
                                        :as "converted-file")
            (document-container         :via      ,(s-prefix "dossier:collectie.bestaatUit")
                                        :inverse t
                                        :as "document-container")
            (piece                      :via      ,(s-prefix "pav:previousVersion")
                                        :as "previous-piece")
            (piece                      :via      ,(s-prefix "pav:previousVersion")
                                        :inverse t
                                        :as "next-piece")
            (subcase                    :via ,(s-prefix "ext:bevatDocumentversie")
                                        :inverse t
                                        :as "subcase")
            (subcase                    :via ,(s-prefix "ext:bevatReedsBezorgdeDocumentversie")
                                        :inverse t
                                        :as "linked-subcase")
            (agendaitem                 :via ,(s-prefix "besluitvorming:geagendeerdStuk")
                                        :inverse t
                                        :as "agendaitem")
            (newsletter-info            :via ,(s-prefix "ext:documentenVoorPublicatie")
                                        :inverse t
                                        :as "newsletter")
            (meeting-record             :via ,(s-prefix "ext:getekendeDocumentVersiesVoorNotulen")
                                        :inverse t
                                        :as "meeting-record")
            (meeting                    :via ,(s-prefix "ext:zittingDocumentversie")
                                        :inverse t
                                        :as "meeting")
            (agenda-item-treatment      :via ,(s-prefix "besluitvorming:genereertVerslag")
                                        :inverse t
                                        :as "agenda-item-treatment")
                                        )
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/stukken/")
  :features `(include-uri)
  :on-path "pieces")

(define-resource document-type ()
  :class (s-prefix "ext:DocumentTypeCode")
  :properties `((:label             :string ,(s-prefix "skos:prefLabel"))
                (:scope-note        :string ,(s-prefix "skos:scopeNote"))
                (:priority          :number ,(s-prefix "ext:prioriteit"))
                (:alt-label         :string ,(s-prefix "skos:altLabel")))
  :has-many `((document-container   :via    ,(s-prefix "ext:documentType")
                                    :inverse t
                                    :as "document-containers")
              (document-type        :via    ,(s-prefix "skos:broader")
                                    :inverse t
                                    :as "subtypes"))
  :has-one `((document-type         :via    ,(s-prefix "skos:broader")
                                    :as "supertype"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/concept/document-type-codes/")
  :features '(include-uri)
  :on-path "document-types")

