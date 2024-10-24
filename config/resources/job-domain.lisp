(define-resource job ()
  :class (s-prefix "cogs:Job")
  :properties `((:created       :datetime  ,(s-prefix "dct:created"))
                (:status        :url       ,(s-prefix "ext:status"))
                (:time-started  :datetime  ,(s-prefix "prov:startedAtTime"))
                (:time-ended    :datetime  ,(s-prefix "prov:endedAtTime"))
                (:message       :string    ,(s-prefix "schema:error")))

  :resource-base (s-url "http://themis.vlaanderen.be/id/jobs/")
  :features '(include-uri)
  :on-path "jobs")

(define-resource file-bundling-job ()
  :class (s-prefix "ext:FileBundlingJob") ; "cogs:Job"
  :properties `((:created       :datetime  ,(s-prefix "dct:created"))
                (:status        :url       ,(s-prefix "ext:status"))
                (:time-started  :datetime  ,(s-prefix "prov:startedAtTime"))
                (:time-ended    :datetime  ,(s-prefix "prov:endedAtTime")))

  :has-one `((file              :via     ,(s-prefix "prov:generated")
                                :as "generated"))
  ; :resource-base (s-url "http://themis.vlaanderen.be/id/file-bundling-jobs/")
  :features '(include-uri)
  :on-path "file-bundling-jobs")

(define-resource document-naming-job ()
  :class (s-prefix "ext:DocumentNamingJob") ; "cogs:Job"
  :properties `((:created       :datetime  ,(s-prefix "dct:created"))
                (:status        :url       ,(s-prefix "ext:status"))
                (:time-started  :datetime  ,(s-prefix "prov:startedAtTime"))
                (:time-ended    :datetime  ,(s-prefix "prov:endedAtTime")))
               ; NOTE: not specifying prov:used here, since we use both agenda and pieces, and mu-cl-resources will not like that
  :features '(include-uri)
  :on-path "document-naming-jobs")

(define-resource publication-metrics-export-job ()
  :class (s-prefix "pub:PublicationMetricsExportJob") ; "cogs:Job"
  :properties `((:created       :datetime  ,(s-prefix "dct:created"))
                (:status        :url       ,(s-prefix "ext:status"))
                (:time-started  :datetime  ,(s-prefix "prov:startedAtTime"))
                (:time-ended    :datetime  ,(s-prefix "prov:endedAtTime"))
                (:config        :string    ,(s-prefix "pub:exportJobConfig"))) ; JSON-blob allowing for extendable filter configuration

  :has-one `((publication-report-type
                                :via     ,(s-prefix "dct:type")
                                :as "report-type")
             (file              :via     ,(s-prefix "prov:generated")
                                :as "generated")
             (user              :via     ,(s-prefix "prov:wasStartedBy")
                                :as "generated-by"))
  :resource-base (s-url "http://themis.vlaanderen.be/id/publicatierapport-export-taak/")
  :features '(include-uri)
  :on-path "publication-metrics-export-jobs")


(define-resource publication-report-type () ; two terms are in use for the same feature: publication-reports and publication-metrics-export. publication-reports is preferred.
  :class (s-prefix "pub:Publicatierapporttype")
  :properties `((:label         :string ,(s-prefix "skos:prefLabel")))

  :resource-base (s-url "http://themis.vlaanderen.be/id/concept/publicatierapporttype/")
  :features '(include-uri)
  :on-path "publication-report-types")
