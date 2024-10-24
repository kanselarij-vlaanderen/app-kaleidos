(define-resource document-container ()
  :class (s-prefix "dossier:Serie")
  :properties `((:created               :datetime ,(s-prefix "dct:created"))
                (:position              :integer ,(s-prefix "schema:position")))
  :has-many `((piece                    :via ,(s-prefix "dossier:Collectie.bestaatUit")
                                        :as "pieces"))
  :has-one `((document-type             :via ,(s-prefix "dct:type")
                                        :as "type"))
  :resource-base (s-url "http://themis.vlaanderen.be/id/serie/")
  :features '(include-uri)
  :on-path "document-containers")

;; Generieke “serie” wordt gebruikt voor specifieke doeleinden voor het groeperen van stukken (KAS-1558)
(define-resource piece ()
  :class (s-prefix "dossier:Stuk")
  :properties `((:name                  :string   ,(s-prefix "dct:title"))
                (:created               :datetime ,(s-prefix "dct:created"))
                (:modified              :datetime ,(s-prefix "dct:modified"))
                (:received-date         :datetime ,(s-prefix "fabio:hasDateReceived"))
                (:number-of-pages       :integer   ,(s-prefix "fabio:hasPageCount"))
                (:number-of-words       :integer   ,(s-prefix "prism:wordCount"))
                (:access-level-last-modified          :datetime  ,(s-prefix "ext:accessLevelLastModified"))
                (:original-name         :string  ,(s-prefix "dct:alternative"))
                (:stamp                 :string  ,(s-prefix "ext:stamp")))
  :has-one `((concept              :via ,(s-prefix "besluitvorming:vertrouwelijkheidsniveau")
                                        :as "access-level")
            (file                       :via      ,(s-prefix "prov:value")
                                        :as "file") ;; make this hasMany for publications
            (document-container         :via      ,(s-prefix "dossier:Collectie.bestaatUit")
                                        :inverse t
                                        :as "document-container")
            (piece                      :via      ,(s-prefix "pav:previousVersion")
                                        :as "previous-piece")
            (piece                      :via      ,(s-prefix "pav:previousVersion")
                                        :inverse t
                                        :as "next-piece")
            (piece                      :via      ,(s-prefix "sign:ongetekendStuk") ;; instead of using signed-piece
                                        :as "unsigned-piece")
            (piece                      :via      ,(s-prefix "sign:ongetekendStuk") ;; instead of using signed-piece
                                        :inverse t
                                        :as "signed-piece")
            (piece                      :via      ,(s-prefix "sign:getekendStukKopie")
                                        :as "signed-piece-copy")
            (piece                      :via      ,(s-prefix "sign:getekendStukKopie")
                                        :inverse t
                                        :as "signed-piece-copy-of")
            (draft-piece                :via      ,(s-prefix "subm:ingediendAlsVoorlopigStuk")
                                        :as "draft-piece")
            (subcase                    :via ,(s-prefix "ext:bevatReedsBezorgdeDocumentversie") ;; should be hasMany, not used in frontend yet
                                        :inverse t
                                        :as "linked-subcase")
            (news-item                  :via ,(s-prefix "besluitvorming:heeftBijlage") ;; Added on export to Themis. Only available in Kaleidos for legacy data.
                                        :inverse t
                                        :as "news-item")
            (meeting                    :via ,(s-prefix "ext:zittingDocumentversie")
                                        :inverse t
                                        :as "meeting")
            ;; (meeting                    :via ,(s-prefix "dossier:genereert") ;; this relation exists in legacy data, but we do not show this in the frontend currently
            ;;                             :inverse t
            ;;                             :as "meeting-notes") ;; note: check if these pieces have a document-container
            (decision-activity          :via ,(s-prefix "besluitvorming:beschrijft") ;; A relationship BehandelingVanAgendapunt -> genereertVerslag -> Stuk exists too in besluitvorming applicatieprofiel. We only implement besluitvorming:beschrijft for now.
                                        :as "decision-activity")
            (language                   :via  ,(s-prefix "dct:language") ;; only when type === translationActivity
                                        :as "language")
            (piece                      :via ,(s-prefix "ext:isVertalingVan") ;; niet eli:is_translation_of, is enkel voor rechtsgeldige documenten !
                                        :as "translation-source")
            ;; publication flow
            (publication-flow           :via ,(s-prefix "pub:referentieDocument")
                                        :inverse t
                                        :as "publication-flow")
            (translation-activity       :via ,(s-prefix "pub:vertalingGenereert")
                                        :inverse t
                                        :as "translation-activity-generated-by")
            (proofing-activity          :via ,(s-prefix "pub:drukproefGebruikt")
                                        :inverse t
                                        :as "publication-activity-used-by")
            (proofing-activity          :via ,(s-prefix "pub:drukproefGenereert")
                                        :inverse t
                                        :as "proofing-activity-generated-by")
            ;; sign flow
            (sign-marking-activity      :via ,(s-prefix "sign:gemarkeerdStuk")
                                        :inverse t
                                        :as "sign-marking-activity")
            (submission-activity        :via ,(s-prefix "prov:generated")
                                        :inverse t
                                        :as "submission-activity")
            (subcase                    :via ,(s-prefix "ext:heeftBekrachtiging")
                                        :inverse t
                                        :as "ratification-subcase")
  )
  :has-many `((case                     :via ,(s-prefix "dossier:Dossier.bestaatUit")
                                        :inverse t
                                        :as "cases")
              (piece                    :via ,(s-prefix "ext:isVertalingVan")
                                        :inverse t
                                        :as "translations")
              (agendaitem               :via ,(s-prefix "besluitvorming:geagendeerdStuk")
                                        :inverse t
                                        :as "agendaitems")
              (agendaitem               :via ,(s-prefix "ext:bevatReedsBezorgdAgendapuntDocumentversie")
                                        :inverse t
                                        :as "linked-agendaitems")
              (request-activity         :via ,(s-prefix "pub:aanvraagGebruikt")
                                        :inverse t
                                        :as "request-activities-used-by")
              (translation-activity     :via ,(s-prefix "pub:drukproefGebruikt")
                                        :inverse t
                                        :as "translation-activities-used-by")
              (proofing-activity        :via ,(s-prefix "pub:drukproefGebruikt")
                                        :inverse t
                                        :as "proofing-activities-used-by")
              (publication-activity     :via ,(s-prefix "pub:publicatieGebruikt")
                                        :inverse t
                                        :as "publication-activities-used-by")
              (submitted-piece          :via ,(s-prefix "parl:heeftStuk")
                                        :inverse t
                                        :as "submitted-pieces")
              (retrieved-piece          :via ,(s-prefix "parl:heeftOpgehaaldStuk")
                                        :inverse t
                                        :as "retrieved-pieces")
  )
  :resource-base (s-url "http://themis.vlaanderen.be/id/stuk/")
  :features `(include-uri)
  :on-path "pieces")

(define-resource minutes (piece)
  :class (s-prefix "ext:Notulen")
  :has-one `((meeting                   :via ,(s-prefix "besluitvorming:heeftNotulen")
                                        :inverse t
                                        :as "minutes-for-meeting"))
  :has-many `((piece-part               :via ,(s-prefix "dct:isPartOf")
                                        :inverse t
                                        :as "piece-parts"))
  :resource-base (s-url "http://themis.vlaanderen.be/id/notulen/")
  :features `(include-uri)
  :on-path "minutes")

(define-resource report (piece)
  :class (s-prefix "besluitvorming:Verslag")
  :has-many `((piece-part               :via ,(s-prefix "dct:isPartOf")
                                        :inverse t
                                        :as "piece-parts")
  )
  :resource-base (s-url "http://themis.vlaanderen.be/id/verslag/")
  :features `(include-uri)
  :on-path "reports")


(define-resource piece-part ()
  :class (s-prefix "dossier:Stukonderdeel")
  :properties `((:title                 :string   ,(s-prefix "dct:title"))
                (:html-content          :string   ,(s-prefix "prov:value"))
                (:created               :datetime ,(s-prefix "dct:created")))
  :has-one `((report                    :via      ,(s-prefix "dct:isPartOf")
                                        :as "report")
             (minutes                   :via      ,(s-prefix "dct:isPartOf")
                                        :as "minutes")
             (piece-part                :via      ,(s-prefix "pav:previousVersion")
                                        :as "previous-piece-part")
             (piece-part                :via      ,(s-prefix "pav:previousVersion")
                                        :inverse t
                                        :as "next-piece-part"))
  :resource-base (s-url "http://themis.vlaanderen.be/id/stukonderdeel/")
  :features `(include-uri)
  :on-path "piece-parts")
