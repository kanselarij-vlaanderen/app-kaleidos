(define-resource submission ()
  :class (s-prefix "subm:Indiening")
  :properties `((:short-title         :string ,(s-prefix "dct:alternative"))
                (:title               :string ,(s-prefix "dct:title"))
                (:confidential        :boolean   ,(s-prefix "ext:vertrouwelijk"))
                (:subcase-name        :string ,(s-prefix "ext:procedurestapNaam"))
                (:created             :datetime ,(s-prefix "dct:created"))
                (:modified            :datetime ,(s-prefix "ext:modified"))
                (:approved-by         :uri-set ,(s-prefix "subm:goedkeurder"))
                (:approval-comment    :string ,(s-prefix "subm:goedkeuringOpmerking"))
                (:notified            :uri-set ,(s-prefix "subm:genotificeerde"))
                (:notification-comment :string ,(s-prefix "subm:ccOpmerking")))
  :has-one `((decisionmaking-flow     :via ,(s-prefix "subm:ingediendVoor")
                                      :as "decisionmaking-flow")
             (subcase                 :via ,(s-prefix "subm:ingediendVoorProcedurestap")
                                      :as "subcase")
             (subcase-type            :via ,(s-prefix "dct:type")
                                      :as "type")
             (concept                 :via ,(s-prefix "ext:agendapuntType")
                                      :as "agenda-item-type")
             (concept                 :via ,(s-prefix "adms:status")
                                      :as "status")
             (mandatee                :via ,(s-prefix "ext:indiener") ;; Opmerking: deze wordt automatisch ingesteld door te kijken wat de mandatee van de organisatie van de gebruiker is
                                      :as "requested-by")
             (user                    :via ,(s-prefix "ext:modifiedBy")
                                      :as "modified-by")
             (user                    :via ,(s-prefix "subm:wordtBehandeldDoor")
                                      :as "being-treated-by")
             (meeting                 :via ,(s-prefix "subm:ingediendVoorVergadering")
                                      :as "meeting"))
  :has-many `((mandatee               :via ,(s-prefix "ext:heeftBevoegde")
                                      :as "mandatees")
              (submission-activity    :via ,(s-prefix "subm:ingediendAls")
                                      :inverse t
                                      :as "submission-activities")
              (submission-status-change-activity :via ,(s-prefix "subm:statusVerandering")
                                      :as "status-change-activities")
              (concept                :via ,(s-prefix "besluitvorming:beleidsveld")
                                      :as "government-areas")
              (draft-piece            :via ,(s-prefix "subm:heeftStuk")
                                      :as "draft-pieces"))
  :resource-base (s-url "http://themis.vlaanderen.be/id/indiening/")
  :features '(include-uri)
  :on-path "submissions")

(define-resource submission-status-change-activity (activity)
  :class (s-prefix "subm:StatusVeranderingsActiviteit")
  :properties `((:started-at          :datetime,(s-prefix "prov:startedAtTime"))
                (:comment             :string ,(s-prefix "schema:comment")))
  :has-one `((submission              :via ,(s-prefix "subm:statusVerandering")
                                      :inverse t
                                      :as "submission")
             (concept                 :via ,(s-prefix "generiek:bewerking")
                                      :as "status"))
  :resource-base (s-url "http://themis.vlaanderen.be/id/indiening-status-verandering/")
  :features '(include-uri)
  :on-path "submission-status-change-activities")

(define-resource draft-document-container ()
  :class (s-prefix "subm:Serie")
  :properties `((:created               :datetime ,(s-prefix "dct:created"))
                (:position              :integer ,(s-prefix "schema:position")))
  :has-many `((draft-piece                    :via ,(s-prefix "subm:Collectie.bestaatUit")
                                        :as "pieces"))
  :has-one `((document-type             :via ,(s-prefix "dct:type")
                                        :as "type"))
  :resource-base (s-url "http://themis.vlaanderen.be/id/voorlopige-serie/")
  :features '(include-uri)
  :on-path "draft-document-containers")

(define-resource draft-piece ()
  :class (s-prefix "subm:VoorlopigStuk")
  :properties `((:name                  :string   ,(s-prefix "dct:title"))
                (:created               :datetime ,(s-prefix "dct:created"))
                (:modified              :datetime ,(s-prefix "dct:modified")))
  :has-one `((concept                   :via ,(s-prefix "besluitvorming:vertrouwelijkheidsniveau")
                                        :as "access-level")
             (draft-file                :via      ,(s-prefix "prov:value")
                                        :as "file")
             (draft-document-container  :via      ,(s-prefix "subm:Collectie.bestaatUit")
                                        :inverse t
                                        :as "document-container")
             (piece                     :via      ,(s-prefix "subm:previousVersion")
                                        :as "previous-piece")
             (submission                :via ,(s-prefix "subm:heeftStuk")
                                        :inverse t
                                        :as "submission"))
  :resource-base (s-url "http://themis.vlaanderen.be/id/voorlopig-stuk/")
  :features `(include-uri)
  :on-path "draft-pieces")

(define-resource draft-file ()
 :class (s-prefix "subm:VoorlopigBestand")
 :properties `((:filename      :string     ,(s-prefix "nfo:fileName"))
               (:format        :string     ,(s-prefix "dct:format"))
               (:size          :integer    ,(s-prefix "nfo:fileSize"))
               (:extension     :string     ,(s-prefix "dbpedia:fileExtension"))
               (:created       :datetime   ,(s-prefix "dct:created")))
 :has-one `((draft-file        :via        ,(s-prefix "nie:dataSource")
                               :inverse t
                               :as "download")
            (draft-file        :via        ,(s-prefix "prov:hadPrimarySource")
                               :as "source")
            (draft-file        :via        ,(s-prefix "prov:hadPrimarySource")
                               :inverse t
                               :as "derived"))
  :resource-base (s-url "http://themis.vlaanderen.be/id/voorlopig-bestand/")
  :features `(no-pagination-defaults include-uri)
  :on-path "draft-files")
