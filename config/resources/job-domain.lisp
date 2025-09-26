;; one of the reasons we want to know about a job in frontend is polling the job vs waiting for a service to execute a call
;; with polling we don't hit http timeouts if a job takes longer to execute
;; if all jobs inherit the main job model, we only need to implement 1 type of job polling
(define-resource job ()
  :class (s-prefix "cogs:Job")
  :properties `((:created       :datetime  ,(s-prefix "dct:created"))
                (:status        :url       ,(s-prefix "adms:status"))
                (:time-started  :datetime  ,(s-prefix "prov:startedAtTime")) ;; when the job got the "ongoing" status (immediately or updating from "scheduled")
                (:time-ended    :datetime  ,(s-prefix "prov:endedAtTime")) ;; when the job is finished with a "success" or "fail" status
                (:message       :string    ,(s-prefix "schema:error"))) ;; message could also be set on partial success/fail

  ;; dct:source is added context for the specific job, what resource was also needed initiate the job
  ;; since subclasses use a different rdfs:Resource, we can't declare specifics
  ;; :has-one `((any-resource       :via       ,(s-prefix "dct:source")
  ;;                                :as "source"))

  ;; prov:generated is a list of created resources during the job
  ;; not all jobs generate a resource, some generate multiple so hasMany
  ;; :has-many `((any-resource             :via       ,(s-prefix "prov:generated")
  ;;                                       :as "generated"))

  ;; prov:used is a list of resources used for the job
  ;; since subclasses use a different rdfs:Resource, we can't declare specifics
  ;; :has-many `((any-resource     :via       ,(s-prefix "prov:used")
  ;;                               :as "used"))

  :resource-base (s-url "http://themis.vlaanderen.be/id/jobs/")
  :features '(include-uri)
  :on-path "jobs")

(define-resource file-bundling-job (job)
  :class (s-prefix "ext:FileBundlingJob") ; "cogs:Job"
  ;; :has-one `((collection     :via     ,(s-prefix "prov:used") ;; defined below
  ;;                            :as "used"))
  :has-one `((file              :via     ,(s-prefix "prov:generated")
                                :as "generated"))
  ; :resource-base (s-url "http://mu.semte.ch/services/file-bundling-service/file-bundling-jobs/")
  :features '(include-uri)
  :on-path "file-bundling-jobs")

;; (define-resource collection ()
;;   :class (s-prefix "prov:Collection")
;;   :properties `((:sha256        :string  ,(s-prefix "ext:sha256"))) ;; based on all the members, no duplicate collections with same members (per profile graph)
;;   ;; we only create a job and a collection after checking if a collection whith the same sha256 does not exist, so 1 job per collection
;;   ;; if any of the members get removed we remove the job and the collection in file-bundling-service (through deltas)
;;   :has-one `((file-bundling-job :via     ,(s-prefix "prov:used")
;;                                 :inverse t
;;                                 :as "file-bundling-job"))
;;   :has-many `((file             :via     ,(s-prefix "prov:hadMember")
;;                                 :as "members"))
;;   ; :resource-base (s-url "http://mu.semte.ch/services/file-bundling-service/collections/")
;;   :features '(include-uri)
;;   :on-path "collections")

(define-resource document-naming-job (job)
  :class (s-prefix "ext:DocumentNamingJob") ; "cogs:Job"
  ;; set in service, not used in frontend currently
  ;; :has-one `((agenda            :via       ,(s-prefix "dct:source")
  ;;                               :as "source"))
  ;; :has-many `((piece            :via       ,(s-prefix "prov:used")
  ;;                               :as "used"))
  :resource-base (s-url "http://mu.semte.ch/services/document-naming/document-naming-jobs/")
  :features '(include-uri)
  :on-path "document-naming-jobs")

(define-resource document-stamping-job (job)
  :class (s-prefix "ext:FileStampingJob") ; "cogs:Job"
  ;; set in service, not used in frontend currently
  ;; :has-many `((file              :via     ,(s-prefix "prov:used") ;; files that should be stamped
  ;;                                :as "used")
  ;;             (file              :via     ,(s-prefix "prov:generated") ;; stamped files
  ;;                                :as "generated"))
  :resource-base (s-url "http://mu.semte.ch/services/document-stamping-service/document-stamping-jobs/")
  :features '(include-uri)
  :on-path "document-stamping-jobs")

(define-resource publication-metrics-export-job (job)
  :class (s-prefix "pub:PublicationMetricsExportJob") ; "cogs:Job"
  :properties `((:config        :string    ,(s-prefix "pub:exportJobConfig"))) ; JSON-blob allowing for extendable filter configuration
  :has-one `((publication-report-type
                                :via     ,(s-prefix "dct:type") 
                                :as "report-type") ;; could be used as a concept
             (file              :via     ,(s-prefix "prov:generated")
                                :as "generated")
             (user              :via     ,(s-prefix "prov:wasStartedBy")
                                :as "generated-by"))
  :resource-base (s-url "http://themis.vlaanderen.be/id/publicatierapport-export-taak/")
  :features '(include-uri)
  :on-path "publication-metrics-export-jobs")


(define-resource publication-report-type ()
  :class (s-prefix "pub:Publicatierapporttype") ;; this is also a skos:concept
  :properties `((:label         :string ,(s-prefix "skos:prefLabel")))

  :resource-base (s-url "http://themis.vlaanderen.be/id/concept/publicatierapporttype/")
  :features '(include-uri)
  :on-path "publication-report-types")


;; for future reference, these models are not used in frontend

;; (define-resource send-to-vp-job () ;; also a "cogs:Job" in spirit
;;   :class (s-prefix "ext:SendToVpJob")
;;   ;; shares properties with cogs:Job but inheritance is not possible because of graph conflicts
;;   ;; This model is persisted on <http://mu.semte.ch/graphs/system/parliamentc>
;;   :properties `((:created       :datetime  ,(s-prefix "dct:created"))
;;                 (:status        :url       ,(s-prefix "adms:status"))
;;                 (:time-started  :datetime  ,(s-prefix "prov:startedAtTime")) ;; when the job got the "ongoing" status (immediately or updating from "scheduled")
;;                 (:time-ended    :datetime  ,(s-prefix "prov:endedAtTime")) ;; when the job is finished with a "success" or "fail" status
;;                 (:message       :string    ,(s-prefix "schema:error"))) ;; message could also be set on partial success/fail
;;   :has-one `((send-to-vp-job-context  :via       ,(s-prefix "prov:used") ;; defined below
;;                                       :as "used"))
;;   ;; :has-many `((file                :via     ,(s-prefix "prov:used") ;; files that should be stamped
;;   ;;                                  :as "used")
;;   ;;             (file                :via     ,(s-prefix "prov:generated") ;; stamped files
;;   ;;                                  :as "generated"))
;;   :resource-base (s-url "http://mu.semte.ch/services/vlaams-parlement-sync/send-to-parliament-job/")
;;   :features '(include-uri)
;;   :on-path "send-to-vp-jobs")

;; (define-resource send-to-vp-job-context ()
;;   :class (s-prefix "ext:SendToVpJobContext")
;;   ;; This model is persisted on <http://mu.semte.ch/graphs/system/parliamentc>
;;   :properties `((:is-complete     :boolean   ,(s-prefix "ext:isComplete"))
;;                 (:comment         :string    ,(s-prefix "ext:comment")))
;;   :has-one `((agendaitem          :via       ,(s-prefix "ext:agendaitem")
;;                                   :as "agendaitem")
;;             (user                 :via       ,(s-prefix "ext:agendaitem")
;;                                   :as "user"))
;;   :has-many `((piece              :via       ,(s-prefix "ext:user")
;;                                   :as "pieces"))
;;   :resource-base (s-url "http://mu.semte.ch/services/vlaams-parlement-sync/send-to-parliament-job-context/")
;;   :features '(include-uri)
;;   :on-path "send-to-vp-job-contexts")


;; (define-resource public-export-job() ;; also a "cogs:Job" in spirit
;;   :class (s-prefix "ext:PublicExportJob")
;;   ;; shares properties with cogs:Job but inheritance is not possible because of graph conflicts
;;   ;; This model is persisted on <http://mu.semte.ch/graphs/themis-public>
;;   :properties `((:created           :datetime     ,(s-prefix "dct:created"))
;;                 (:status            :url          ,(s-prefix "adms:status"))
;;                 (:time-started      :datetime     ,(s-prefix "prov:startedAtTime")) ;; when the job got the "ongoing" status (immediately or updating from "scheduled")
;;                 (:time-ended        :datetime     ,(s-prefix "prov:endedAtTime")) ;; when the job is finished with a "success" or "fail" status
;;                 (:message           :string       ,(s-prefix "schema:error"))  ;; message could also be set on partial success/fail
;;                 (:retry-count       :integer      ,(s-prefix "ext:retryCount")) ;; retry several times before failing
;;                 (:scope             :string-set   ,(s-prefix "ext:scope")) ;; ["newsitems"], ["newsitems", "documents"] or none / 
;;                 )
;;   :has-one `((meeting               :via          ,(s-prefix "prov:used")
;;                                     :as "used")
;;              (activity              :via          ,(s-prefix "prov:generated") ;; this does not a have subclass of activity
;;                                     :as "generated")
;;              (themis-publication-activity  :via   ,(s-prefix "dct:source")
;;                                     :as "source"))
;;   :resource-base (s-url "http://data.kaleidos.vlaanderen.be/public-export-jobs/")
;;   :features '(include-uri)
;;   :on-path "public-export-jobs")



