defmodule Dispatcher do
  use Matcher

  define_accept_types [
    json: [ "application/json", "application/vnd.api+json" ],
    html: [ "text/html", "application/xhtml+html"],
    css: [ "text/css" ],
    any: [ "*/*" ],
  ]

  define_layers [ :api, :frontend ]

  @frontend_html %{ accept: [ :html ], layer: :frontend }
  @frontend_css %{ accept: [ :css ], layer: :frontend }
  @frontend_any %{ accept: [ :any ], layer: :frontend }
  @json_service %{ accept: [ :json ], layer: :api }

  ### Files

  get "/files/:id/download", %{ layer: :api } do
    Proxy.forward conn, [], "http://file/files/" <> id <> "/download"
  end

  post "/files/*path", %{ layer: :api } do
    Proxy.forward conn, path, "http://file/files/"
  end

  delete "/files/*path", %{ layer: :api } do
    Proxy.forward conn, path, "http://file/files/"
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

  match "/cases/search/*path", @json_service do
    Proxy.forward conn, path, "http://search/cases/search/"
  end

  match "/cases/index/*path", @json_service do
    Proxy.forward conn, path, "http://search/cases/search/"
  end

  match "/cases/invalidate/*path", @json_service do
    Proxy.forward conn, path, "http://search/cases/invalidate/"
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
  match "/decision-result-codes/*path", @json_service do
    Proxy.forward conn, path, "http://cache/decision-result-codes/"
  end
  match "/bestuurseenheden/*path", @json_service do
    Proxy.forward conn, path, "http://cache/bestuurseenheden/"
  end
  match "/werkingsgebieden/*path", @json_service do
    Proxy.forward conn, path, "http://cache/werkingsgebieden/"
  end
  match "/bestuurseenheid-classificatie-codes/*path", @json_service do
    Proxy.forward conn, path, "http://cache/bestuurseenheid-classificatie-codes/"
  end
  match "/bestuursorgaan-classificatie-codes/*path", @json_service do
    Proxy.forward conn, path, "http://cache/bestuursorgaan-classificatie-codes/"
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

  match "/document-types/*path", @json_service do
    Proxy.forward conn, path, "http://cache/document-types/"
  end

  match "/document-type-codes/*path", @json_service do
    Proxy.forward conn, path, "http://cache/document-type-codes/"
  end
  match "/media-type-codes/*path", @json_service do
    Proxy.forward conn, path, "http://cache/media-type-codes/"
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
  match "/submission-activities/*path", @json_service do
    Proxy.forward conn, path, "http://cache/submission-activities/"
  end
  match "/access-levels/*path", @json_service do
    Proxy.forward conn, path, "http://cache/access-levels/"
  end
  match "/approvals/*path", @json_service do
    Proxy.forward conn, path, "http://cache/approvals/"
  end

  match "/file-addresses/*path", @json_service do
    Proxy.forward conn, path, "http://cache/file-addresses/"
  end

  get "/concepts/*path", @json_service do
    Proxy.forward conn, path, "http://cache/concepts/"
  end
  get "/concept-schemes/*path", @json_service do
    Proxy.forward conn, path, "http://cache/concept-schemes/"
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

  match "/time-periods/*path", @json_service do
    Proxy.forward conn, path, "http://cache/time-periods/"
  end

  match "/organizations/*path", @json_service do
    Proxy.forward conn, path, "http://cache/organizations/"
  end

  match "/newsletter-infos/search/*path", @json_service do
    Proxy.forward conn, path, "http://search/newsletter-infos/search/"
  end

  match "/newsletter-infos/*path", @json_service do
    Proxy.forward conn, path, "http://cache/newsletter-infos/"
  end

  match "/themis-publication-activities/*path", @json_service do
    Proxy.forward conn, path, "http://cache/themis-publication-activities/"
  end

  match "/themes/*path", @json_service do
    Proxy.forward conn, path, "http://cache/themes/"
  end

  match "/users/*path", @json_service do
    Proxy.forward conn, path, "http://cache/users/"
  end

  match "/accounts/*path", @json_service do
    Proxy.forward conn, path, "http://cache/accounts/"
  end

  match "/agenda-comparison/*path", @json_service  do
    Proxy.forward conn, path, "http://agenda-comparison/"
  end

  match "/custom-subcases/*path", @json_service do
    Proxy.forward conn, path, "http://custom-subcases/"
  end

  match "/account-groups/*path", @json_service do
    Proxy.forward conn, path, "http://cache/account-groups/"
  end

  match "/mock/sessions/*path", @json_service do
    Proxy.forward conn, path, "http://mocklogin/sessions/"
  end
  match "/sessions/*path", @json_service do
    Proxy.forward conn, path, "http://login/sessions/"
  end

  match "/alerts/*path", @json_service do
    Proxy.forward conn, path, "http://cache/alerts/"
  end

  match "/user-management/*path", @json_service do
    Proxy.forward conn, path, "http://user-management/"
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
    Proxy.forward conn, path, "http://cache/publication-statuses/"
  end

  get "/urgency-levels/*path", @json_service do
    Proxy.forward conn, path, "http://cache/urgency-levels/"
  end

  match "/publication-status-changes/*path", @json_service do
    Proxy.forward conn, path, "http://cache/publication-status-changes/"
  end

  get "/publication-modes/*path", @json_service do
    Proxy.forward conn, path, "http://cache/publication-modes/"
  end

  match "/regulation-types/*path", @json_service do
    Proxy.forward conn, path, "http://cache/regulation-types/"
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

  match "/cancellation-activities/*path", @json_service do
    Proxy.forward conn, path, "http://cache/cancellation-activities/"
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

  match "/sign-refusal-activities/*path", @json_service do
    Proxy.forward conn, path, "http://cache/sign-refusal-activities/"
  end

  match "/sign-cancellation-activities/*path", @json_service do
    Proxy.forward conn, path, "http://cache/sign-cancellation-activities/"
  end

  match "/sign-completion-activities/*path", @json_service do
    Proxy.forward conn, path, "http://cache/sign-completion-activities/"
  end

  match "/signed-pieces/*path", @json_service do
    Proxy.forward conn, path, "http://cache/signed-pieces/"
  end

  get "/signing-flows/:signing_flow_id/pieces", @json_service do
    Proxy.forward conn, [], "http://digital-signing/signing-flows/" <> signing_flow_id <> "/pieces"
  end

  post "/signing-flows/:signing_flow_id/upload-document-to-signinghub", @json_service do
    Proxy.forward conn, [], "http://digital-signing/signing-flows/" <> signing_flow_id <> "/upload-document-to-signinghub"
  end

  post "/sign-flows/:signing_flow_id/pieces/:piece_id/signers", @json_service do
    Proxy.forward conn, [], "http://digital-signing/signing-flows/" <> signing_flow_id <> "/pieces/" <> piece_id <> "/signers"
  end

  get "/signing-flows/:signing_flow_id/pieces/:piece_id/signinghub-url", @json_service do
    Proxy.forward conn, [], "http://digital-signing/signing-flows/" <> signing_flow_id <> "/pieces/" <> piece_id <> "/signinghub-url"
  end

  post "/signing-flows/:signing_flow_id/start", @json_service do
    Proxy.forward conn, [], "http://digital-signing/signing-flows/" <> signing_flow_id <> "/start"
  end

  get "/mail-folders/*path", @json_service do
    Proxy.forward conn, path, "http://cache/mail-folders/"
  end

  match "/emails/*path", @json_service do
    Proxy.forward conn, path, "http://cache/emails/"
  end

  get "/recovery-status/*path", @json_service do
    Proxy.forward conn, [], "http://database:8890/recovery-status/"
  end

  match "/email-notification-settings/*path", @json_service do
    Proxy.forward conn, path, "http://cache/email-notification-settings/"
  end

  match "/publication-metrics-export-jobs/*path", @json_service do
    Proxy.forward conn, path, "http://cache/publication-metrics-export-jobs/"
  end

  match "/files/*path", @json_service do
    Proxy.forward conn, path, "http://cache/files/"
  end

  ### Frontend

  match "/assets/*path", @frontend_css do
    Proxy.forward conn, path, "http://frontend/assets/"
  end

  match "/assets/*path", @frontend_any do
    Proxy.forward conn, path, "http://frontend/assets/"
  end

  match "/*_path", @frontend_html do
    Proxy.forward conn, [], "http://frontend/index.html"
  end

  ### 404

  match "_", %{ last_call: true } do
    send_resp( conn, 404, "Route not found.  See config/dispatcher.ex" )
  end

end
