;;;;;;;;;;;;;;;;;;;
;;; delta messenger
(in-package :delta-messenger)

;; (push (make-instance 'delta-logging-handler) *delta-handlers*)
(add-delta-messenger "http://delta-notifier/")


;;;;;;;;;;;;;;;;;
;;; configuration
(in-package :client)
(setf *log-sparql-query-roundtrip* t)
(setf *backend* "http://triplestore:8890/sparql"
      ;; (list "http://triplestore:8890/sparql"
      ;;       "http://triplestore1:8890/sparql"
      ;;       "http://triplestore2:8890/sparql"
      ;;       "http://triplestore3:8890/sparql"
      ;;       )
      )

(in-package :server)
(setf *log-incoming-requests-p* t)

;;;;;;;;;;;;;;;;;
;;; access rights

(in-package :acl)

(defparameter *access-specifications* nil
  "All known ACCESS specifications.")

(defparameter *graphs* nil
  "All known GRAPH-SPECIFICATION instances.")

(defparameter *rights* nil
  "All known GRANT instances connecting ACCESS-SPECIFICATION to GRAPH.")

(define-prefixes
  ;; Core
  :mu "http://mu.semte.ch/vocabularies/core/"
  :session "http://mu.semte.ch/vocabularies/session/"
  ;; Custom URIs
  :ext "http://mu.semte.ch/vocabularies/ext/"
  :pub "http://mu.semte.ch/vocabularies/ext/publicatie/"
  :sign "http://mu.semte.ch/vocabularies/ext/handtekenen/"
  :sh "http://mu.semte.ch/vocabularies/ext/signinghub/"
  :parl "http://mu.semte.ch/vocabularies/ext/parlement/"
  ;; (:userroles "http://themis.vlaanderen.be/id/gebruikersrol/")
  ;; Generic ontologies
  :schema "http://schema.org/"
  :foaf "http://xmlns.com/foaf/0.1./"
  :skos "http://www.w3.org/2004/02/skos/core#"
  :person "http://www.w3.org/ns/person#"
  :adms "http://www.w3.org/ns/adms#"
  :prov "http://www.w3.org/ns/prov#"
  :org "http://www.w3.org/ns/org#"
  :cogs "http://vocab.deri.ie/cogs#"
  :nfo "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#"
  :nmo "http://www.semanticdesktop.org/ontologies/2007/03/22/nmo#"
  ;; Flemish ontologies (OSLO)
  :dossier "https://data.vlaanderen.be/ns/dossier#"
  :besluitvorming "https://data.vlaanderen.be/ns/besluitvorming#"
  :besluit "http://data.vlaanderen.be/ns/besluit#"
  :generiek "https://data.vlaanderen.be/ns/generiek#"
  ;; European ontologyies
  :eli "http://data.europa.eu/eli/ontology#"
  :euvoc "http://publications.europa.eu/ontology/euvoc#"
  )

;;;;;;;;;;;;;;;;;;;;
;; Access queries

(defun query-for-authenticated ()
  (format nil "PREFIX org: <http://www.w3.org/ns/org#>
              PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
              SELECT ?role_uri WHERE {
                <SESSION_ID> ext:sessionMembership / org:role ?role_uri .
              } LIMIT 1"))

(defun query-for-roles (roles)
  (format nil "PREFIX org: <http://www.w3.org/ns/org#>
               PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
               SELECT ?role_uri WHERE {
                 <SESSION_ID> ext:sessionMembership / org:role ?ownRole .
                 OPTIONAL { <SESSION_ID> ext:impersonatedRole ?maybeImpersonatedRole . }
                 BIND(COALESCE(?maybeImpersonatedRole, ?ownRole) AS ?role_uri)
                 VALUES ?role_uri { ~{<~A>~^ ~} }
               } LIMIT 1"
          roles))

(defun query-for-own-roles (roles)
  (format nil "PREFIX org: <http://www.w3.org/ns/org#>
               PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
               SELECT ?role_uri WHERE {
                 <SESSION_ID> ext:sessionMembership / org:role ?role_uri .
                 VALUES ?role_uri { ~{<~A>~^ ~} }
               } LIMIT 1"
          roles))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; User roles

(defvar *admin-roles*
  '("http://themis.vlaanderen.be/id/gebruikersrol/9a969b13-e80b-424f-8a82-a402bcb42bc5")) ;; Admin

(defvar *secretarie-roles*
  '("http://themis.vlaanderen.be/id/gebruikersrol/ab39b02a-14a5-4aa9-90bd-e0fa268b0f3d"   ;; Kanselarij
    "http://themis.vlaanderen.be/id/gebruikersrol/c2ef1785-bf28-458f-952d-aa40989347d2")) ;; Secretarie

(defvar *ovrb-roles*
  '("http://themis.vlaanderen.be/id/gebruikersrol/648a1ffe-1a26-4931-a329-18d26a91438f")) ;; OVRB

(defvar *kort-bestek-roles*
  '("http://themis.vlaanderen.be/id/gebruikersrol/ca20a872-7743-4998-b479-06b003f49daf")) ;; Kort Bestek

(defvar *minister-roles*
  '("http://themis.vlaanderen.be/id/gebruikersrol/01ace9e0-f810-474e-b8e0-f578ff1e230d" )) ;; Minister

(defvar *kabinet-dossierbeheerder-roles*
  '("http://themis.vlaanderen.be/id/gebruikersrol/6bcebe59-0cb5-4c5e-ab40-ca98b65887a4")) ;; Kabinet dossierbeheerder

(defvar *kabinet-medewerker-roles*
  '("http://themis.vlaanderen.be/id/gebruikersrol/33dbca4a-7e57-41d2-a26c-aedef422ff84" )) ;; Kabinet medewerker

(defvar *overheid-roles*
  '("http://themis.vlaanderen.be/id/gebruikersrol/06cfd67b-1637-47d3-811f-97aa23a83644"   ;; Overheidsorganisatie
    "http://themis.vlaanderen.be/id/gebruikersrol/12543581-7f02-4166-87d2-ab15ddfce642")) ;; Vlaams Parlement

;;;;;;;;;;;;;;;;;;
;; Always accessible read graphs for all visitors

(define-graph public ("http://mu.semte.ch/graphs/public")
  ("ext:SysteemNotificatie" -> _))

(define-graph sessions ("http://mu.semte.ch/graphs/sessions")
  ("session:Session" -> _))

(define-graph staatsblad ("http://mu.semte.ch/graphs/staatsblad"))

(supply-allowed-group "public")

(grant (read)
       :to public
       :for-allowed-group "public")

(grant (read)
       :to sessions
       :for-allowed-group "public")

(grant (read)
       :to staatsblad
       :for-allowed-group "public")

;;;;;;;;;;;;;;;;;;
;; Accessible to all authenticated users

(define-graph system/users ("http://mu.semte.ch/graphs/system/users")
  ("foaf:OnlineAccount" -> _)
  ("foaf:Person" -> _)
  ("foaf:Organization" -> _)
  ("org:Membership" -> _)
  ("ext:LoginActivity" -> _))

(supply-allowed-group "authenticated"
                      :query (query-for-authenticated))

(grant (read)
       :to system/users
       :for-allowed-group "authenticated")

;;;;;;;;;;;;;;;;;;
;; System-specific data access for admin users

(supply-allowed-group "admin"
                      :query (query-for-roles *admin-roles*))

(grant (read write)
       :to system/users
       :for-allowed-group "admin")

(grant (read write)
       :to public
       :for-allowed-group "admin")

(supply-allowed-group "impersonation"
                      :query (query-for-own-roles *admin-roles*))
                      ;; :query "PREFIX org: <http://www.w3.org/ns/org#>
                      ;;         PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
                      ;;         SELECT ?role_uri WHERE {
                      ;;             <SESSION_ID> ext:sessionMembership / org:role ?role_uri .
                      ;;             VALUES ?role_uri { ~{~A~^, ~} }
                      ;;         } LIMIT 1")

(grant (read write)
       :to sessions
       :for-allowed-group "impersonation")

;;;;;;;;;;;;;;;;;;;;;;;;
;; Kanselarij, secretarie, OVRB
;; Equivalent to "Intern secretarie" access level w.r.t. documents.
;; All the secretarie & OVRB roles read the same data, but OVRB can only write
;; a limited amount.

(define-graph system/email ("http://mu.semte.ch/graphs/system/email")
  ("nfo:Folder" -> _)
  ("nmo:Email" -> _)
  ("ext:EmailNotificationSettings" -> _))

(define-graph kanselarij ("http://mu.semte.ch/graphs/organizations/kanselarij")
  ("ext:Nieuwsbericht" -> _)
  ("ext:MailCampagne" -> _)
  ("prov:Activity" -> _)
  ("ext:InternalDecisionPublicationActivity" -> _)
  ("ext:InternalDocumentPublicationActivity" -> _)
  ("ext:ThemisPublicationActivity" -> _)
  ("besluitvorming:Agenda" -> _)
  ("besluit:Agendapunt" -> _)
  ("besluit:BehandelingVanAgendapunt" -> _)
  ("besluitvorming:Beslissingsactiviteit" -> _)
  ("besluit:Vergaderactiviteit" -> _)
  ("besluitvorming:Agendering" -> _)
  ("ext:Indieningsactiviteit" -> _)
  ("ext:AgendaStatusActivity" -> _)
  ("dossier:Dossier" -> _)
  ("besluitvorming:Besluitvormingsaangelegenheid" -> _)
  ("dossier:Procedurestap" -> _)
  ("nfo:FileDataObject" -> _)
  ("foaf:Document" -> _)
  ("dossier:Serie" -> _)
  ("ext:DocumentVersie" -> _)
  ("dossier:Stuk" -> _)
  ("dossier:Stukonderdeel" -> _)
  ("ext:Notulen" -> _)
  ("besluitvorming:Verslag" -> _)
  ("prov:Collection" -> _)
  ("cogs:Job" -> _)
  ("ext:FileBundlingJob" -> _)
  ("pub:Publicatieaangelegenheid" -> _)
  ("pub:VertalingProcedurestap" -> _)
  ("pub:PublicatieProcedurestap" -> _)
  ("pub:PublicatieStatusWijziging" -> _)
  ("person:Person" -> _)
  ("schema:ContactPoint" -> _)
  ("org:Organization" -> _)
  ("pub:AanvraagActiviteit" -> _)
  ("pub:VertaalActiviteit" -> _)
  ("pub:DrukproefActiviteit" -> _)
  ("pub:PublicatieActiviteit" -> _)
  ("pub:AnnulatieActiviteit" -> _)
  ("generiek:GestructureerdeIdentificator" -> _)
  ("adms:Identifier" -> _)
  ("pub:PublicationMetricsExportJob" -> _)
  ("eli:LegalResource" -> _)
  ("ext:ReportGenerationJob" -> _))

(define-graph kanselarij-for-ovrb ("http://mu.semte.ch/graphs/organizations/kanselarij")
  ("dossier:Dossier" -> _)
  ("besluitvorming:Besluitvormingsaangelegenheid" -> _)
  ("dossier:Procedurestap" -> _)
  ("nfo:FileDataObject" -> _)
  ("foaf:Document" -> _)
  ("dossier:Serie" -> _)
  ("ext:DocumentVersie" -> _)
  ("dossier:Stuk" -> _)
  ("dossier:Stukonderdeel" -> _)
  ("ext:Notulen" -> _)
  ("besluitvorming:Verslag" -> _)
  ("pub:Publicatieaangelegenheid" -> _)
  ("pub:VertalingProcedurestap" -> _)
  ("pub:PublicatieProcedurestap" -> _)
  ("pub:PublicatieStatusWijziging" -> _)
  ("person:Person" -> _)
  ("schema:ContactPoint" -> _)
  ("org:Organization" -> _)
  ("pub:AanvraagActiviteit" -> _)
  ("pub:VertaalActiviteit" -> _)
  ("pub:DrukproefActiviteit" -> _)
  ("pub:PublicatieActiviteit" -> _)
  ("pub:AnnulatieActiviteit" -> _)
  ("generiek:GestructureerdeIdentificator" -> _)
  ("adms:Identifier" -> _)
  ("pub:PublicationMetricsExportJob" -> _)
  ("eli:LegalResource" -> _))

(define-graph kanselarij ("http://mu.semte.ch/graphs/organizations/kanselarij")
  ("ext:Nieuwsbericht" -> _)
  ("ext:MailCampagne" -> _)
  ("prov:Activity" -> _)
  ("ext:InternalDecisionPublicationActivity" -> _)
  ("ext:InternalDocumentPublicationActivity" -> _)
  ("ext:ThemisPublicationActivity" -> _)
  ("besluitvorming:Agenda" -> _)
  ("besluit:Agendapunt" -> _)
  ("besluit:BehandelingVanAgendapunt" -> _)
  ("besluitvorming:Beslissingsactiviteit" -> _)
  ("besluit:Vergaderactiviteit" -> _)
  ("besluitvorming:Agendering" -> _)
  ("ext:Indieningsactiviteit" -> _)
  ("ext:AgendaStatusActivity" -> _)
  ("dossier:Dossier" -> _)
  ("besluitvorming:Besluitvormingsaangelegenheid" -> _)
  ("dossier:Procedurestap" -> _)
  ("nfo:FileDataObject" -> _)
  ("foaf:Document" -> _)
  ("dossier:Serie" -> _)
  ("ext:DocumentVersie" -> _)
  ("dossier:Stuk" -> _)
  ("dossier:Stukonderdeel" -> _)
  ("ext:Notulen" -> _)
  ("besluitvorming:Verslag" -> _)
  ("prov:Collection" -> _)
  ("cogs:Job" -> _)
  ("ext:FileBundlingJob" -> _)
  ("pub:Publicatieaangelegenheid" -> _)
  ("pub:VertalingProcedurestap" -> _)
  ("pub:PublicatieProcedurestap" -> _)
  ("pub:PublicatieStatusWijziging" -> _)
  ("person:Person" -> _)
  ("schema:ContactPoint" -> _)
  ("org:Organization" -> _)
  ("pub:AanvraagActiviteit" -> _)
  ("pub:VertaalActiviteit" -> _)
  ("pub:DrukproefActiviteit" -> _)
  ("pub:PublicatieActiviteit" -> _)
  ("pub:AnnulatieActiviteit" -> _)
  ("generiek:GestructureerdeIdentificator" -> _)
  ("adms:Identifier" -> _)
  ("pub:PublicationMetricsExportJob" -> _)
  ("eli:LegalResource" -> _)
  ("ext:ReportGenerationJob" -> _))

(supply-allowed-group "kanselarij-read"
                      :query (query-for-roles
                              (concatenate 'list
                                           *admin-roles*
                                           *secretarie-roles*
                                           *ovrb-roles*
                                           *kort-bestek-roles*)))

(supply-allowed-group "kanselarij-write"
                      :query (query-for-roles
                              (concatenate 'list
                                           *admin-roles*
                                           *secretarie-roles*
                                           *kort-bestek-roles*)))

(supply-allowed-group "ovrb-write"
                      :query (query-for-roles
                              (concatenate 'list
                                           *admin-roles*
                                           *ovrb-roles*)))

(grant (read)
       :to kanselarij
       :for-allowed-group "kanselarij-read")

(grant (read)
       :to system/email
       :for-allowed-group "kanselarij-read")

(grant (write)
       :to kanselarij
       :for-allowed-group "kanselarij-write")

(grant (write)
       :to system/email
       :for-allowed-group "kanselarij-write")

(grant (write)
       :to kanselarij-for-ovrb
       :for-allowed-group "ovrb-write")

(grant (write)
       :to system/email
       :for-allowed-group "ovrb-write")

;; Ministers & their cabinet chiefs
;; Equivalent to "Vertrouwelijk" access level for documents

(define-graph minister ("http://mu.semte.ch/graphs/organizations/minister")
  ("nfo:FileDataObject" -> _)
  ("prov:Collection" -> _)
  ("cogs:Job" -> _)
  ("ext:FileBundlingJob" -> _))

(supply-allowed-group "minister-read"
                      :query (query-for-roles
                              (concatenate 'list
                                           *minister-roles*
                                           *kabinet-dossierbeheerder-roles*)))

(supply-allowed-group "minister-write"
                      :query (query-for-roles
                              (concatenate 'list
                                           *minister-roles*
                                           *kabinet-dossierbeheerder-roles*)))

(grant (read)
       :to minister
       :for-allowed-group "minister-read")

(grant (write)
       :to minister
       :for-allowed-group "minister-write")

;; Cabinet staff

(define-graph intern-regering ("http://mu.semte.ch/graphs/organizations/intern-regering")
  ("nfo:FileDataObject" -> _)
  ("prov:Collection" -> _)
  ("cogs:Job" -> _)
  ("ext:FileBundlingJob" -> _))

(supply-allowed-group "regering-read"
                      :query (query-for-roles *kabinet-medewerker-roles*))

(supply-allowed-group "regering-write"
                      :query (query-for-roles *kabinet-medewerker-roles*))

(grant (read)
       :to intern-regering
       :for-allowed-group "regering-read")

(grant (write)
       :to intern-regering
       :for-allowed-group "regering-write")

;; Government staff

(define-graph intern-overheid ("http://mu.semte.ch/graphs/organizations/intern-overheid")
  ("nfo:FileDataObject" -> _)
  ("prov:Collection" -> _)
  ("cogs:Job" -> _)
  ("ext:FileBundlingJob" -> _))

(supply-allowed-group "overheid-read"
                      :query (query-for-roles *overheid-roles*))

(supply-allowed-group "overeid-write"
                      :query (query-for-roles *overheid-roles*))

(grant (read)
       :to intern-overheid
       :for-allowed-group "overheid-read")

(grant (write)
       :to intern-overheid
       :for-allowed-group "overheid-write")

;; Sing flow metadata

(define-graph system/signing ("http://mu.semte.ch/graphs/system/signing")
  ("sign:Handtekenaangelgenheid" -> _)
  ("sign:HandtekenProcedurestap" -> _)
  ("sign:Markeringsactiviteit" -> _)
  ("sign:Voorbereidingsactiviteit" -> _)
  ("sign:Handtekenactiviteit" -> _)
  ("sign:Goedkeuringsactiviteit" -> _)
  ("sign:Weigeractiviteit" -> _)
  ("sign:AnnulatieActiviteit" -> _)
  ("sign:Afrondingsactiviteit" -> _)
  ("sh:Document" -> _))

(supply-allowed-group "sign-flow-read"
                      :query (query-for-roles
                              (concatenate 'list
                                           *admin-roles*
                                           *secretarie-roles*
                                           *ovrb-roles*
                                           *kort-bestek-roles*
                                           *minister-roles*
                                           *kabinet-dossierbeheerder-roles*
                                           *kabinet-medewerker-roles*)))

(supply-allowed-group "sign-flow-write"
                      :query (query-for-roles
                              (concatenate 'list
                                           *admin-roles*
                                           *secretarie-roles*
                                           *kort-bestek-roles*
                                           *minister-roles*
                                           *kabinet-dossierbeheerder-roles*)))

(grant (read)
       :to system/signing
       :for-allowed-group "sign-flow-read")

(grant (write)
       :to system/signing
       :for-allowed-group "sign-flow-write")

;; Parliament flow metadata

(define-graph system/parliament ("http://mu.semte.ch/graphs/system/parliament")
  ("parl:Parlementaireaangelegenheid" -> _)
  ("parl:ParlementaireProcedurestap" -> _)
  ("parl:ParlementaireIndieningsactiviteit" -> _)
  ("parl:ParlementaireOphalingsactiviteit" -> _)
  ("parl:IngediendStuk" -> _)
  ("parl:OpgehaaldStuk" -> _)
  ("ext:SendToVpJob" -> _)
  ("ext:SendToVpJobContext" -> _))

(supply-allowed-group "parliament-flow-read"
                      :query (query-for-roles
                              (concatenate 'list
                                           *admin-roles*
                                           *secretarie-roles*
                                           *ovrb-roles*
                                           *kort-bestek-roles*
                                           *minister-roles*
                                           *kabinet-dossierbeheerder-roles*
                                           *kabinet-medewerker-roles*
                                           *overheid-roles*)))

(supply-allowed-group "parliament-flow-write"
                      :query (query-for-roles
                              (concatenate 'list
                                           *admin-roles*
                                           *minister-roles*
                                           *kabinet-dossierbeheerder-roles*)))

(grant (read)
       :to system/parliament
       :for-allowed-group "parliament-flow-read")

(grant (write)
       :to system/parliament
       :for-allowed-group "parliament-flow-write")

;; READ ACCESS FOR SYNC CONSUMER SERVICE FROM OTHER STACK

(define-graph delta-files ("http://mu.semte.ch/graphs/delta-files")
  ("nfo:FileDataObject" -> _))

(supply-allowed-group "sync-consumer"
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
          PREFIX muAccount: <http://mu.semte.ch/vocabularies/account/>
          PREFIX mu: <http://mu.semte.ch/vocabularies/core/>

          SELECT ?thing WHERE {
            <SESSION_ID> muAccount:account <http://services.lblod.info/diff-consumer/account>.
            VALUES ?thing { \"let me in\" }
          }")

(grant (read)
       :to delta-files
       :for-allowed-group "sync-consumer")

;; Cleanup

;; (define-graph everything ("http://mu.semte.ch/application")
;;   (_ -> _))

;; (supply-allowed-group "clean")

;; (grant (read write)
;;        :to everything
;;        :for-allowed-group "clean")
