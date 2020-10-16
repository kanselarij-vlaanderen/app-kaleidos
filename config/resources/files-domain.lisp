(define-resource file ()
  :class (s-prefix "nfo:FileDataObject")
  :properties `((:filename      :string     ,(s-prefix "nfo:fileName"))
                (:format        :string     ,(s-prefix "dct:format"))
                (:size          :number     ,(s-prefix "nfo:fileSize"))
                (:extension     :string     ,(s-prefix "dbpedia:fileExtension"))
                (:created       :datetime   ,(s-prefix "dct:created"))
                (:content-type  :string     ,(s-prefix "ext:contentType")))
  :has-one `((file              :via        ,(s-prefix "nie:dataSource")
                                :inverse t
                                :as "download")
             (agenda            :via        ,(s-prefix "ext:getekendeNotule") ;; KAS-1465 whats this ?
                                :inverse t
                                :as "agenda")
             (signature         :via        ,(s-prefix "ext:handtekening")
                                :inverse t
                                :as "signature"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/files/")
  :features `(no-pagination-defaults include-uri)
  :on-path "files")
