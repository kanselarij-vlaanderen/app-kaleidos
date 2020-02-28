(define-resource file-bundling-job ()
  :class (s-prefix "ext:FileBundlingJob") ; "cogs:Job"
  :properties `((:created       :datetime  ,(s-prefix "dct:created"))
                (:status        :uri       ,(s-prefix "ext:status"))
                (:time-started  :datetime  ,(s-prefix "prov:startedAtTime"))
                (:time-ended    :datetime  ,(s-prefix "prov:endedAtTime"))
                (:generated     :uri       ,(s-prefix "prov:generated"))
  )
  :has-one `((file              :via     ,(s-prefix "prov:generated")
                                :as "generated"))
  ; :resource-base (s-url "http://kanselarij.vo.data.gift/id/file-bundling-jobs/")
  :features '(include-uri)
  :on-path "file-bundling-jobs"
)
