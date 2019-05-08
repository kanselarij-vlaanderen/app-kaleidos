;;;;
;; NOTE
;; docker-compose stop; docker-compose rm; docker-compose up
;; after altering this file.

(in-package :mu-cl-resources)
(defparameter *cache-model-properties-p* nil)
(defparameter *cache-count-queries* nil)
(defparameter *supply-cache-headers-p* t
  "when non-nil, cache headers are supplied.  this works together with mu-cache.")
(setf *cache-model-properties-p* nil)
(defparameter *include-count-in-paginated-responses* t
  "when non-nil, all paginated listings will contain the number
   of responses in the result object's meta.")
(defparameter *max-group-sorted-properties* nil)

(read-domain-file "besluit-domain.lisp")
(read-domain-file "besluitvorming-domain.lisp")
(read-domain-file "document-domain.lisp")
(read-domain-file "dossier-domain.lisp")
(read-domain-file "files-domain.lisp")
(read-domain-file "mandaat-domain.lisp")
(read-domain-file "organisatie-domain.lisp")
(read-domain-file "publicatie-domain.lisp")
(read-domain-file "alerts-domain.lisp")
(read-domain-file "users-domain.lisp")

