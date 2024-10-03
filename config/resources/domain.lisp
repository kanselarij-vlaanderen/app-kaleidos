;;;;
;; NOTE
;; docker-compose stop; docker-compose rm; docker-compose up
;; after altering this file.

(in-package :mu-cl-resources)
(defparameter *cache-model-properties-p* t)
(defparameter *cache-count-queries* t)
(defparameter *supply-cache-headers-p* t
  "when non-nil, cache headers are supplied.  this works together with mu-cache.")
(defparameter *include-count-in-paginated-responses* t
  "when non-nil, all paginated listings will contain the number
   of responses in the result object's meta.")
(defparameter *max-group-sorted-properties* nil)
;; (defparameter sparql:*query-log-types* nil)
(defparameter sparql:*query-log-types* nil)  ;; '(:default :update-group :update :query :ask)

(in-package :sparql)
(defparameter *experimental-no-application-graph-for-sudo-select-queries* t)
(defparameter *no-application-graph-for-sudo-select-queries* t)

(in-package :mu-cl-resources)
(read-domain-file "activity-domain.lisp")
(read-domain-file "agenda-activities-domain.json")
(read-domain-file "alerts-domain.lisp")
(read-domain-file "besluit-domain.json")
(read-domain-file "besluit-domain.lisp")
(read-domain-file "besluitvorming-domain.lisp")
(read-domain-file "document-domain.lisp")
(read-domain-file "dossier-domain.lisp")
(read-domain-file "files-domain.lisp")
(read-domain-file "handtekening-domain.json")
(read-domain-file "job-domain.lisp")
(read-domain-file "mandaat-domain.lisp")
(read-domain-file "newsletter-domain.json")
(read-domain-file "organisatie-domain.json")
(read-domain-file "publicatie-domain.json")
(read-domain-file "submissions-domain.lisp")
(read-domain-file "meeting-activities-domain.json")
(read-domain-file "users-domain.json")
(read-domain-file "email-domain.lisp")
(read-domain-file "generiek-domain.json")
(read-domain-file "concept-domain.json")
(read-domain-file "health-check.lisp")
(read-domain-file "parliament-domain.json")
