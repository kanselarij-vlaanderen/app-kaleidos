defmodule Dispatcher do
  use Matcher

  define_accept_types [
    json: [ "application/json", "application/vnd.api+json" ],
    html: [ "text/html", "application/xhtml+html"],
    css: [ "text/css" ],
    any: [ "*/*" ],
  ]

  define_layers [ :frontend, :api, :not_found ]

  @frontend %{ accept: [ :any ], layer: :frontend }
  @json_service %{ accept: [ :json ], layer: :api }
  @not_found %{ accept: [ :any ], layer: :not_found }

  ### Frontend

  get "/assets/*path", @frontend do
    Proxy.forward conn, path, "http://frontend/assets/"
  end

  get "/@appuniversum/*path", @frontend do
    Proxy.forward conn, path, "http://frontend/@appuniversum/"
  end

  get "/authorization/callback", @frontend do
    Proxy.forward conn, [], "http://frontend/torii/redirect.html"
  end

  get "/handleiding", @frontend do
    Proxy.forward conn, [], "http://static-file/handleiding.pdf"
  end

  get "/leidraad-digitaal-ondertekenen", @frontend do
    Proxy.forward conn, [], "http://static-file/leidraad-digitaal-ondertekenen.pdf"
  end

  ### Health check endpoint
  get "/health-checks/*_path", @json_service do
    forward conn, [], "http://resource/health-checks/"
  end

  ### File conversion

  post "/files/:id/convert", @json_service do
    Proxy.forward conn, [], "http://docx-conversion/files/" <> id <> "/convert"
  end

  ### Files

  get "/files/:id/download", %{ layer: :api } do
    Proxy.forward conn, [], "http://file/files/" <> id <> "/download"
  end

  post "/files/*path", %{ layer: :api } do
    Proxy.forward conn, path, "http://file/files/"
  end

  delete "/files/*path", @json_service do
    Proxy.forward conn, path, "http://file/files/"
  end

  post "/draft-files/:id/move", @json_service do
    Proxy.forward conn, [], "http://draft-file-mover/draft-files/" <> id <> "/move"
  end

  get "/draft-files/:id/download", %{ layer: :api } do
    Proxy.forward conn, [], "http://draft-file/files/" <> id <> "/download"
  end

  post "/draft-files/*path", %{ layer: :api } do
    Proxy.forward conn, path, "http://draft-file/files/"
  end

  delete "/draft-files/*path", @json_service do
    Proxy.forward conn, path, "http://draft-file/files/"
  end

  get "/ember-pdfjs-wrapper/*path", @frontend do
    Proxy.forward conn, path, "http://frontend/ember-pdfjs-wrapper/"
  end

  ### Mirror sync producer

  match "/sync/*path", %{} do
    Proxy.forward conn, path, "http://delta-producer/"
  end

  match "/fileshare/*path", %{} do
    Proxy.forward conn, path, "http://fileshare/download/"
  end

  ### Search

  match "/agendaitems/search/*path", @json_service do
    Proxy.forward conn, path, "http://search/agendaitems/search/"
  end

  match "/agendaitems/index/*path", @json_service do
    Proxy.forward conn, path, "http://search/agendaitems/search/"
  end

  match "/agendaitems/invalidate/*path", @json_service do
    Proxy.forward conn, path, "http://search/agendaitems/invalidate/"
  end

  match "/decisionmaking-flows/search/*path", @json_service do
    Proxy.forward conn, path, "http://search/decisionmaking-flows/search/"
  end

  match "/decisionmaking-flows/index/*path", @json_service do
    Proxy.forward conn, path, "http://search/decisionmaking-flows/search/"
  end

  match "/decisionmaking-flows/invalidate/*path", @json_service do
    Proxy.forward conn, path, "http://search/decisionmaking-flows/invalidate/"
  end

  match "/pieces/search/*path", @json_service do
    Proxy.forward conn, path, "http://search/pieces/search/"
  end


  match "/search/settings/*path", @json_service do
    Proxy.forward conn, path, "http://search/settings/"
  end


  ### Document version management

  put "/agendaitems/:id/pieces", @json_service do
    Proxy.forward conn, [], "http://document-versions/agendaitems/" <> id <> "/documents"
  end

  put "/agendaitems/:id/pieces/restore", @json_service do
    Proxy.forward conn, [], "http://document-versions/agendaitems/" <> id <> "/pieces/restore"
  end


  ### Agenda comparison

  get "/agendas/:agenda_id/compare/:compared_agenda_id/agenda-items", @json_service do
    Proxy.forward conn, [], "http://agenda-comparison/agendas/" <> agenda_id <> "/compare/" <> compared_agenda_id <> "/agenda-items"
  end

  get "/agendas/:agenda_id/compare/:compared_agenda_id/agenda-item/:agenda_item_id/pieces", @json_service do
    Proxy.forward conn, [], "http://agenda-comparison/agendas/" <> agenda_id <> "/compare/" <> compared_agenda_id <> "/agenda-item/" <> agenda_item_id <> "/documents"
  end

  post "/agendas/:id/agendaitems/pieces/files/archive", @json_service do
    Proxy.forward conn, [], "http://file-bundling-job-creation/agendas/" <> id <> "/agendaitems/documents/files/archive"
  end


  ### Agenda approval and meeting management

  post "/meetings/:meeting_id/reopen", @json_service do
    Proxy.forward conn, [], "http://agenda-approve/meetings/" <> meeting_id <> "/reopen"
  end

  post "/meetings/:meeting_id/close", @json_service do
    Proxy.forward conn, [], "http://agenda-approve/meetings/" <> meeting_id <> "/close"
  end

  post "/agendas/:agenda_id/approve", @json_service do
    Proxy.forward conn, [], "http://agenda-approve/agendas/" <> agenda_id <> "/approve"
  end

  post "/agendas/:agenda_id/reopen", @json_service do
    Proxy.forward conn, [], "http://agenda-approve/agendas/" <> agenda_id <> "/reopen"
  end

  post "/agendas/:agenda_id/close", @json_service do
    Proxy.forward conn, [], "http://agenda-approve/agendas/" <> agenda_id <> "/close"
  end

  delete "/agendas/:agenda_id", @json_service do
    Proxy.forward conn, [], "http://agenda-approve/agendas/" <> agenda_id
  end

  ### Submission of subcases on meeting

  post "/meetings/:meeting_id/submit", @json_service do
    Proxy.forward conn, [], "http://agenda-submission/meetings/" <> meeting_id <> "/submit"
  end

  post "/meetings/:meeting_id/submit-submission", @json_service do
    Proxy.forward conn, [], "http://agenda-submission/meetings/" <> meeting_id <> "/submit-submission"
  end

  get "/meetings/open", @json_service do
    Proxy.forward conn, [], "http://agenda-submission/open-meetings"
  end

  get "/submissions/:submission_id/for-meeting", @json_service do
    Proxy.forward conn, [], "http://agenda-submission/submissions/" <> submission_id <> "/for-meeting"
  end

  post "/agendas/:agenda_id/reorder", @json_service do
    Proxy.forward conn, [], "http://agenda-submission/agendas/" <> agenda_id <> "/reorder"
  end

  ### Themis export

  post "/meetings/:meeting_id/themis-export" do
    Proxy.forward conn, [], "http://themis-export/meetings/" <> meeting_id <> "/publication-activities"
  end

  get "/public-export-jobs/*path" do
    Proxy.forward conn, path, "http://themis-export/public-export-jobs/"
  end

  get "/publications/*path" do
    Proxy.forward conn, path, "http://publication-producer/files/"
  end

  get "/public-files/*path" do
    Proxy.forward conn, path, "http://public-file/files/"
  end

  get "/export-files/*path" do
    Proxy.forward conn, path, "http://export-file/files/"
  end

  ### Minutes report generation

  match "/generate-minutes-report/*path", @json_service do
    Proxy.forward conn, path, "http://minutes-report-generation/"
  end

  ### Document shortlists

  get "/publication-flows/shortlist/*path", @json_service do
    Proxy.forward conn, path, "http://shortlist/publication-flows/"
  end

  get "/sign-flows/shortlist/*path", @json_service do
    Proxy.forward conn, path, "http://shortlist/sign-flows/"
  end

  ### Authentication

  match "/mock/sessions/*path", @json_service do
    Proxy.forward conn, path, "http://mocklogin/sessions/"
  end

  match "/sessions/*path", @json_service do
    Proxy.forward conn, path, "http://login/sessions/"
  end

  match "/users/*path", @json_service do
    Proxy.forward conn, path, "http://cache/users/"
  end

  get "/accounts/*path", @json_service do
    Proxy.forward conn, path, "http://cache/accounts/"
  end

  match "/user-organizations/*path", @json_service do
    Proxy.forward conn, path, "http://cache/user-organizations/"
  end

  match "/memberships/*path", @json_service do
    Proxy.forward conn, path, "http://cache/memberships/"
  end

  get "/login-activities/*path", @json_service do
    Proxy.forward conn, path, "http://cache/login-activities/"
  end

  match "/impersonations/*path", @json_service do
    Proxy.forward conn, path, "http://impersonation/impersonations/"
  end

  ### Regular resources and cache

  match "/agendas/*path", @json_service do
    Proxy.forward conn, path, "http://cache/agendas/"
  end
  match "/agendaitems/*path", @json_service do
    Proxy.forward conn, path, "http://cache/agendaitems/"
  end
  match "/agenda-item-treatments/*path", @json_service do
    Proxy.forward conn, path, "http://cache/agenda-item-treatments/"
  end
  match "/decision-activities/*path", @json_service do
    Proxy.forward conn, path, "http://cache/decision-activities/"
  end

  match "/meetings/*path", @json_service do
    Proxy.forward conn, path, "http://cache/meetings/"
  end

  match "/document-containers/*path", @json_service do
    Proxy.forward conn, path, "http://cache/document-containers/"
  end

  match "/pieces/*path", @json_service do
    Proxy.forward conn, path, "http://cache/pieces/"
  end

  match "/reports/*path", @json_service do
    Proxy.forward conn, path, "http://cache/reports/"
  end

  match "/minutes/*path", @json_service do
    Proxy.forward conn, path, "http://cache/minutes/"
  end

  match "/piece-parts/*path", @json_service do
    Proxy.forward conn, path, "http://cache/piece-parts/"
  end

  match "/decisionmaking-flows/*path", @json_service do
    Proxy.forward conn, path, "http://cache/decisionmaking-flows/"
  end
  match "/cases/*path", @json_service do
    Proxy.forward conn, path, "http://cache/cases/"
  end

  match "/case-types/*path", @json_service do
    Proxy.forward conn, path, "http://cache/case-types/"
  end

  match "/subcases/*path", @json_service do
    Proxy.forward conn, path, "http://cache/subcases/"
  end
  match "/agendastatuses/*path", @json_service do
    Proxy.forward conn, path, "http://cache/agendastatuses/"
  end
  match "/subcase-types/*path", @json_service do
    Proxy.forward conn, path, "http://cache/subcase-types/"
  end
  match "/agenda-activities/*path", @json_service do
    Proxy.forward conn, path, "http://cache/agenda-activities/"
  end
  match "/agenda-status-activities/*path", @json_service do
    Proxy.forward conn, path, "http://cache/agenda-status-activities/"
  end
  match "/submission-activities/*path", @json_service do
    Proxy.forward conn, path, "http://cache/submission-activities/"
  end

  get "/concepts/*path", @json_service do
    Proxy.forward conn, path, "http://forever-cache/concepts/"
  end
  get "/concept-schemes/*path", @json_service do
    Proxy.forward conn, path, "http://forever-cache/concept-schemes/"
  end
  get "/document-types/*path", @json_service do
    Proxy.forward conn, path, "http://cache/document-types/"
  end
  match "/mandatees/*path", @json_service do
    Proxy.forward conn, path, "http://cache/mandatees/"
  end
  get "/mandates/*path", @json_service do
    Proxy.forward conn, path, "http://cache/mandates/"
  end
  get "/roles/*path", @json_service do
    Proxy.forward conn, path, "http://cache/roles/"
  end
  get "/government-bodies/*path", @json_service do
    Proxy.forward conn, path, "http://cache/government-bodies/"
  end
  get "/generations/*path", @json_service do
    Proxy.forward conn, path, "http://cache/generations/"
  end
  get "/invalidations/*path", @json_service do
    Proxy.forward conn, path, "http://cache/invalidations/"
  end
  match "/persons/*path", @json_service do
    Proxy.forward conn, path, "http://cache/persons/"
  end
  match "/contact-persons/*path", @json_service do
    Proxy.forward conn, path, "http://cache/contact-persons/"
  end

  match "/identifications/*path", @json_service do
    Proxy.forward conn, path, "http://cache/identifications/"
  end

  match "/structured-identifiers/*path", @json_service do
    Proxy.forward conn, path, "http://cache/structured-identifiers/"
  end

  match "/organizations/*path", @json_service do
    Proxy.forward conn, path, "http://cache/organizations/"
  end

  match "/news-items/search/*path", @json_service do
    Proxy.forward conn, path, "http://search/news-items/search/"
  end

  match "/news-items/*path", @json_service do
    Proxy.forward conn, path, "http://cache/news-items/"
  end

  match "/internal-decision-publication-activities/*path", @json_service do
    Proxy.forward conn, path, "http://cache/internal-decision-publication-activities/"
  end

  match "/internal-document-publication-activities/*path", @json_service do
    Proxy.forward conn, path, "http://cache/internal-document-publication-activities/"
  end

  match "/themis-publication-activities/*path", @json_service do
    Proxy.forward conn, path, "http://cache/themis-publication-activities/"
  end

  match "/themes/*path", @json_service do
    Proxy.forward conn, path, "http://cache/themes/"
  end

  match "/agenda-comparison/*path", @json_service  do
    Proxy.forward conn, path, "http://agenda-comparison/"
  end

  match "/alerts/*path", @json_service do
    Proxy.forward conn, path, "http://cache/alerts/"
  end

  match "/alert-types/*path", @json_service do
    Proxy.forward conn, path, "http://cache/alert-types"
  end

  match "/shortcuts/*path", @json_service do
    Proxy.forward conn, path, "http://cache/shortcuts/"
  end

  match "/newsletter/*path", @json_service do
    Proxy.forward conn, path, "http://newsletter/"
  end

  match "/mail-campaigns/*path", @json_service do
    Proxy.forward conn, path, "http://cache/mail-campaigns/"
  end

  match "/file-bundling-jobs/*path", @json_service do
    Proxy.forward conn, path, "http://cache/file-bundling-jobs/"
  end

  get "/document-naming-jobs/*path", @json_service do
    Proxy.forward conn, path, "http://resource/document-naming-jobs/"
  end

  get "/jobs/*path", @json_service do
    Proxy.forward conn, path, "http://cache/jobs/"
  end

  # PUBLICATION-FLOW
  match "/publication-flows/search/*path", @json_service do
    Proxy.forward conn, path, "http://search/publication-flows/search/"
  end

  match "/publication-flows/*path", @json_service do
    Proxy.forward conn, path, "http://cache/publication-flows/"
  end

  match "/publication-subcases/*path", @json_service do
    Proxy.forward conn, path, "http://cache/publication-subcases/"
  end

  match "/translation-subcases/*path", @json_service do
    Proxy.forward conn, path, "http://cache/translation-subcases/"
  end

  match "/publication-statuses/*path", @json_service do
    Proxy.forward conn, path, "http://forever-cache/publication-statuses/"
  end

  get "/urgency-levels/*path", @json_service do
    Proxy.forward conn, path, "http://forever-cache/urgency-levels/"
  end

  match "/publication-status-changes/*path", @json_service do
    Proxy.forward conn, path, "http://cache/publication-status-changes/"
  end

  get "/publication-modes/*path", @json_service do
    Proxy.forward conn, path, "http://forever-cache/publication-modes/"
  end

  match "/regulation-types/*path", @json_service do
    Proxy.forward conn, path, "http://forever-cache/regulation-types/"
  end

  match "/request-activities/*path", @json_service do
    Proxy.forward conn, path, "http://cache/request-activities/"
  end

  match "/translation-activities/*path", @json_service do
    Proxy.forward conn, path, "http://cache/translation-activities/"
  end

  match "/proofing-activities/*path", @json_service do
    Proxy.forward conn, path, "http://cache/proofing-activities/"
  end

  match "/publication-activities/*path", @json_service do
    Proxy.forward conn, path, "http://cache/publication-activities/"
  end

  match "/decisions/*path", @json_service do
    Proxy.forward conn, path, "http://cache/decisions/"
  end

  get "/languages/*path", @json_service do
    Proxy.forward conn, path, "http://cache/languages/"
  end

  # SIGN FLOW
  match "/sign-flows/*path", @json_service do
    Proxy.forward conn, path, "http://cache/sign-flows/"
  end

  match "/sign-subcases/*path", @json_service do
    Proxy.forward conn, path, "http://cache/sign-subcases/"
  end

  match "/sign-marking-activities/*path", @json_service do
    Proxy.forward conn, path, "http://cache/sign-marking-activities/"
  end

  match "/sign-preparation-activities/*path", @json_service do
    Proxy.forward conn, path, "http://cache/sign-preparation-activities/"
  end

  match "/sign-signing-activities/*path", @json_service do
    Proxy.forward conn, path, "http://cache/sign-signing-activities/"
  end

  match "/sign-approval-activities/*path", @json_service do
    Proxy.forward conn, path, "http://cache/sign-approval-activities/"
  end

  match "/sign-refusal-activities/*path", @json_service do
    Proxy.forward conn, path, "http://cache/sign-refusal-activities/"
  end

  match "/sign-cancellation-activities/*path", @json_service do
    Proxy.forward conn, path, "http://cache/sign-cancellation-activities/"
  end

  match "/sign-completion-activities/*path", @json_service do
    Proxy.forward conn, path, "http://cache/sign-completion-activities/"
  end

  post "/signing-flows/upload-to-signinghub", @json_service do
    Proxy.forward conn, [], "http://digital-signing/signing-flows/upload-to-signinghub"
  end

  get "/signing-flows/:signing_flow_id/pieces/:piece_id/signinghub-url", @json_service do
    Proxy.forward conn, [], "http://digital-signing/signing-flows/" <> signing_flow_id <> "/pieces/" <> piece_id <> "/signinghub-url"
  end

  delete "/signing-flows/:signing_flow_id", @json_service do
    Proxy.forward conn, [], "http://digital-signing/signing-flows/" <> signing_flow_id
  end

  get "/digital-signing/health-check", @json_service do
    Proxy.forward conn, [], "http://digital-signing/verify-credentials"
  end

  post "/signing-flows/mark-pieces-for-signing", @json_service do
    Proxy.forward conn, [], "http://digital-signing/signing-flows/mark-pieces-for-signing"
  end

  get "/signing-flows/job/:id", @json_service do
    Proxy.forward conn, [], "http://digital-signing/job/" <> id
  end

  get "/mail-folders/*path", @json_service do
    Proxy.forward conn, path, "http://cache/mail-folders/"
  end

  match "/emails/*path", @json_service do
    Proxy.forward conn, path, "http://cache/emails/"
  end

  get "/recovery-status/*_path", @json_service do
    Proxy.forward conn, [], "http://database:8890/recovery-status/"
  end

  match "/email-notification-settings/*path", @json_service do
    Proxy.forward conn, path, "http://cache/email-notification-settings/"
  end

  match "/publication-metrics-export-jobs/*path", @json_service do
    Proxy.forward conn, path, "http://cache/publication-metrics-export-jobs/"
  end

  get "/publication-report-types/*path", @json_service do
    Proxy.forward conn, path, "http://cache/publication-report-types/"
  end

  match "/files/*path", @json_service do
    Proxy.forward conn, path, "http://cache/files/"
  end

  match "/draft-files/*path", @json_service do
    Proxy.forward conn, path, "http://cache/draft-files/"
  end


  ### Decision extraction
  match "/decision-extraction/*path", @json_service do
    Proxy.forward conn, path, "http://decision-extraction/"
  end

  ### Decision report generation
  match "/generate-decision-report/*path", @json_service do
    Proxy.forward conn, path, "http://decision-report-generation/"
  end

  ### Vlaams Parlement
  match "/parliament-flows/*path", @json_service do
    Proxy.forward conn, path, "http://cache/parliament-flows/"
  end
  match "/parliament-subcases/*path", @json_service do
    Proxy.forward conn, path, "http://cache/parliament-subcases/"
  end
  match "/parliament-submission-activities/*path", @json_service do
    Proxy.forward conn, path, "http://cache/parliament-submission-activities/"
  end
  match "/submitted-pieces/*path", @json_service do
    Proxy.forward conn, path, "http://cache/submitted-pieces/"
  end
  match "/parliament-retrieval-activities/*path", @json_service do
    Proxy.forward conn, path, "http://cache/parliament-retrieval-activities/"
  end
  match "/retrieved-pieces/*path", @json_service do
    Proxy.forward conn, path, "http://cache/retrieved-pieces/"
  end
  match "/vlaams-parlement-sync/*path", @json_service do
    Proxy.forward conn, path, "http://vlaams-parlement-sync/"
  end

  ### Cabinet submissions
  match "/submissions/*path", @json_service do
    Proxy.forward conn, path, "http://cache/submissions/"
  end

  match "/submission-status-change-activities/*path", @json_service do
    Proxy.forward conn, path, "http://cache/submission-status-change-activities/"
  end

  match "/draft-document-containers/*path", @json_service do
    Proxy.forward conn, path, "http://cache/draft-document-containers/"
  end

  match "/draft-pieces/*path", @json_service do
    Proxy.forward conn, path, "http://cache/draft-pieces/"
  end

  match "/submission-internal-reviews/*path", @json_service do
    Proxy.forward conn, path, "http://cache/submission-internal-reviews/"
  end

  ### Document Naming
  match "/document-naming/*path", @json_service do
    Proxy.forward conn, path, "http://document-naming/"
  end

  ### Document Stamping
  match "/document-stamping/*path", @json_service do
    Proxy.forward conn, path, "http://document-stamping/"
  end

  ## Fallback

  get "/*_path", %{ layer: :api, accept: %{ html: true } } do
    Proxy.forward conn, [], "http://frontend/index.html"
  end

  match "/*_path", %{ layer: :not_found, accept: %{ json: true } } do
    send_resp( conn, 404, "{ \"error\": { \"code\": 404, \"message\": \"Route not found.  See config/dispatcher.ex\" } }" )
  end

  match "/*_path", @not_found do
    send_resp( conn, 404, "Route not found. See config/dispatcher.ex" )
  end

 end
