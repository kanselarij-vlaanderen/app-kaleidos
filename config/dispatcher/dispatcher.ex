defmodule Dispatcher do
  use Matcher

  define_accept_types [
    html: [ "text/html", "application/xhtml+html" ],
    json: [ "application/json", "application/vnd.api+json" ],
  ]

  @any %{}
  @json %{ accept: %{ json: true } }

  match "/agendaitems/search/*path", @any do
    Proxy.forward conn, path, "http://musearch/agendaitems/search/"
  end

  match "/agendaitems/index/*path", @any do
    Proxy.forward conn, path, "http://musearch/agendaitems/search/"
  end

  match "/agendaitems/invalidate/*path", @any do
    Proxy.forward conn, path, "http://musearch/agendaitems/invalidate/"
  end

  match "/cases/search/*path", @any do
    Proxy.forward conn, path, "http://musearch/cases/search/"
  end

  match "/cases/index/*path", @any do
    Proxy.forward conn, path, "http://musearch/cases/search/"
  end

  match "/cases/invalidate/*path", @any do
    Proxy.forward conn, path, "http://musearch/cases/invalidate/"
  end

  match "/musearch/settings/*path", @any do
    Proxy.forward conn, path, "http://musearch/settings/"
  end

  put "/agendaitems/:id/pieces", @any do
    Proxy.forward conn, [], "http://document-versions-service/agendaitems/" <> id <> "/documents"
  end

  put "/agendaitems/:id/pieces/restore", @any do
    Proxy.forward conn, [], "http://document-versions-service/agendaitems/" <> id <> "/pieces/restore"
  end

  get "/agendas/:agenda_id/compare/:compared_agenda_id/agenda-items", @any do
    Proxy.forward conn, [], "http://agenda-sort-service/agendas/" <> agenda_id <> "/compare/" <> compared_agenda_id <> "/agenda-items"
  end

  get "/agendas/:agenda_id/compare/:compared_agenda_id/agenda-item/:agenda_item_id/pieces", @any do
    Proxy.forward conn, [], "http://agenda-sort-service/agendas/" <> agenda_id <> "/compare/" <> compared_agenda_id <> "/agenda-item/" <> agenda_item_id <> "/documents"
  end

  post "/agendas/:id/agendaitems/pieces/files/archive", @any do
    Proxy.forward conn, [], "http://file-bundling-job-creation-service/agendas/" <> id <> "/agendaitems/documents/files/archive"
  end
  match "/agendas/*path", @any do
    Proxy.forward conn, path, "http://cache/agendas/"
  end
  match "/agendaitems/*path", @any do
    Proxy.forward conn, path, "http://cache/agendaitems/"
  end
  match "/agenda-item-treatments/*path", @any do
    Proxy.forward conn, path, "http://cache/agenda-item-treatments/"
  end
  match "/decision-result-codes/*path", @any do
    Proxy.forward conn, path, "http://cache/decision-result-codes/"
  end
  match "/bestuurseenheden/*path", @any do
    Proxy.forward conn, path, "http://cache/bestuurseenheden/"
  end
  match "/werkingsgebieden/*path", @any do
    Proxy.forward conn, path, "http://cache/werkingsgebieden/"
  end
  match "/bestuurseenheid-classificatie-codes/*path", @any do
    Proxy.forward conn, path, "http://cache/bestuurseenheid-classificatie-codes/"
  end
  match "/bestuursorgaan-classificatie-codes/*path", @any do
    Proxy.forward conn, path, "http://cache/bestuursorgaan-classificatie-codes/"
  end
  match "/meetings/*path", @any do
    Proxy.forward conn, path, "http://cache/meetings/"
  end

  match "/document-containers/*path", @any do
    Proxy.forward conn, path, "http://cache/document-containers/"
  end
  match "/pieces/*path", @any do
    Proxy.forward conn, path, "http://cache/pieces/"
  end

  match "/document-types/*path", @any do
    Proxy.forward conn, path, "http://cache/document-types/"
  end

  match "/document-type-codes/*path", @any do
    Proxy.forward conn, path, "http://cache/document-type-codes/"
  end
  match "/media-type-codes/*path", @any do
    Proxy.forward conn, path, "http://cache/media-type-codes/"
  end
  match "/cases/*path", @any do
    Proxy.forward conn, path, "http://cache/cases/"
  end

  match "/case-types/*path", @any do
    Proxy.forward conn, path, "http://cache/case-types/"
  end

  match "/subcases/*path", @any do
    Proxy.forward conn, path, "http://cache/subcases/"
  end
  match "/agendastatuses/*path", @any do
    Proxy.forward conn, path, "http://cache/agendastatuses/"
  end
  match "/subcase-types/*path", @any do
    Proxy.forward conn, path, "http://cache/subcase-types/"
  end
  match "/agenda-activities/*path", @any do
    Proxy.forward conn, path, "http://cache/agenda-activities/"
  end
  match "/submission-activities/*path", @any do
    Proxy.forward conn, path, "http://cache/submission-activities/"
  end
  match "/access-levels/*path", @any do
    Proxy.forward conn, path, "http://cache/access-levels/"
  end
  match "/approvals/*path", @any do
    Proxy.forward conn, path, "http://cache/approvals/"
  end

  match "/file-addresses/*path", @any do
    Proxy.forward conn, path, "http://cache/file-addresses/"
  end

  get "/concepts/*path", @any do
    Proxy.forward conn, path, "http://cache/concepts/"
  end
  get "/concept-schemes/*path", @any do
    Proxy.forward conn, path, "http://cache/concept-schemes/"
  end
  match "/mandatees/*path", @any do
    Proxy.forward conn, path, "http://cache/mandatees/"
  end
  get "/mandates/*path", @any do
    Proxy.forward conn, path, "http://cache/mandates/"
  end
  get "/roles/*path", @any do
    Proxy.forward conn, path, "http://cache/roles/"
  end
  get "/government-bodies/*path", @any do
    Proxy.forward conn, path, "http://cache/government-bodies/"
  end
  match "/persons/*path", @any do
    Proxy.forward conn, path, "http://cache/persons/"
  end
  match "/contact-persons/*path", @any do
    Proxy.forward conn, path, "http://cache/contact-persons/"
  end

  match "/identifications/*path", @any do
    Proxy.forward conn, path, "http://cache/identifications/"
  end

  match "/structured-identifiers/*path", @any do
    Proxy.forward conn, path, "http://cache/structured-identifiers/"
  end

  match "/time-periods/*path", @any do
    Proxy.forward conn, path, "http://cache/time-periods/"
  end

  match "/organizations/*path", @any do
    Proxy.forward conn, path, "http://cache/organizations/"
  end

  match "/newsletter-infos/search/*path", @any do
    Proxy.forward conn, path, "http://musearch/newsletter-infos/search/"
  end

  match "/newsletter-infos/*path", @any do
    Proxy.forward conn, path, "http://cache/newsletter-infos/"
  end

  match "/themis-publication-activities/*path", @any do
    Proxy.forward conn, path, "http://cache/themis-publication-activities/"
  end

  match "/themes/*path", @any do
    Proxy.forward conn, path, "http://cache/themes/"
  end

  match "/users/*path", @any do
    Proxy.forward conn, path, "http://cache/users/"
  end

  match "/accounts/*path", @any do
    Proxy.forward conn, path, "http://cache/accounts/"
  end

  match "/agenda-sort/*path", @any do
    Proxy.forward conn, path, "http://agenda-sort-service/"
  end

  match "/custom-subcases/*path", @any do
    Proxy.forward conn, path, "http://custom-subcases-service/"
  end

  match "/session-service/*path", @any do
    Proxy.forward conn, path, "http://session-number-service/"
  end

  match "/agenda-approve/*path", @any do
    Proxy.forward conn, path, "http://agenda-approve-service/"
  end

  match "/account-groups/*path", @any do
    Proxy.forward conn, path, "http://cache/account-groups/"
  end

  get "/files/:id/download", @any do
    Proxy.forward conn, [], "http://file/files/" <> id <> "/download"
  end

  post "/files/*path", @any do
    Proxy.forward conn, path, "http://file/files/"
  end

  delete "/files/*path", @any do
    Proxy.forward conn, path, "http://file/files/"
  end

  match "/files/*path", @any do
    Proxy.forward conn, path, "http://cache/files/"
  end

  match "/mock/sessions/*path", @any do
    Proxy.forward conn, path, "http://mocklogin/sessions/"
  end
  match "/sessions/*path", @any do
    Proxy.forward conn, path, "http://login/sessions/"
  end

  match "/alerts/*path", @any do
    Proxy.forward conn, path, "http://cache/alerts/"
  end

  match "/user-management-service/*path", @any do
    Proxy.forward conn, path, "http://user-management-service/"
  end

  match "/alert-types/*path", @any do
    Proxy.forward conn, path, "http://cache/alert-types"
  end

  match "/shortcuts/*path", @any do
    Proxy.forward conn, path, "http://cache/shortcuts/"
  end

  match "/newsletter/*path", @any do
    Proxy.forward conn, path, "http://newsletter-service/"
  end

  match "/mail-campaigns/*path", @any do
    Proxy.forward conn, path, "http://cache/mail-campaigns/"
  end

  match "/file-bundling-jobs/*path", @any do
    Proxy.forward conn, path, "http://cache/file-bundling-jobs/"
  end

  match "/mandatee-service/*path", @any do
    Proxy.forward conn, path, "http://mandatee-service/"
  end

  # PUBLICATION-FLOW
  match "/publication-flows/search/*path", @any do
    Proxy.forward conn, path, "http://musearch/publication-flows/search/"
  end

  match "/publication-flows/*path", @any do
    Proxy.forward conn, path, "http://cache/publication-flows/"
  end

  match "/publication-subcases/*path", @any do
    Proxy.forward conn, path, "http://cache/publication-subcases/"
  end

  match "/translation-subcases/*path", @any do
    Proxy.forward conn, path, "http://cache/translation-subcases/"
  end

  match "/publication-statuses/*path", @any do
    Proxy.forward conn, path, "http://cache/publication-statuses/"
  end

  get "/urgency-levels/*path", @any do
    Proxy.forward conn, path, "http://cache/urgency-levels/"
  end

  match "/publication-status-changes/*path", @any do
    Proxy.forward conn, path, "http://cache/publication-status-changes/"
  end

  get "/publication-modes/*path", @any do
    Proxy.forward conn, path, "http://cache/publication-modes/"
  end

  match "/regulation-types/*path", @any do
    Proxy.forward conn, path, "http://cache/regulation-types/"
  end

  match "/request-activities/*path", @any do
    Proxy.forward conn, path, "http://cache/request-activities/"
  end

  match "/translation-activities/*path", @any do
    Proxy.forward conn, path, "http://cache/translation-activities/"
  end

  match "/proofing-activities/*path", @any do
    Proxy.forward conn, path, "http://cache/proofing-activities/"
  end

  match "/publication-activities/*path", @any do
    Proxy.forward conn, path, "http://cache/publication-activities/"
  end

  match "/cancellation-activities/*path", @any do
    Proxy.forward conn, path, "http://cache/cancellation-activities/"
  end

  match "/decisions/*path", @any do
    Proxy.forward conn, path, "http://cache/decisions/"
  end

  get "/languages/*path", @any do
    Proxy.forward conn, path, "http://cache/languages/"
  end

  # TODO: remove endpoint
  get "/publication-report-dev/*path", @any do
    Proxy.forward conn, path, "http://publication-report/"
  end

  # SIGN FLOW
  match "/sign-flows/*path", @any do
    Proxy.forward conn, path, "http://cache/sign-flows/"
  end

  match "/sign-subcases/*path", @any do
    Proxy.forward conn, path, "http://cache/sign-subcases/"
  end

  match "/sign-marking-activities/*path", @any do
    Proxy.forward conn, path, "http://cache/sign-marking-activities/"
  end

  match "/sign-preparation-activities/*path", @any do
    Proxy.forward conn, path, "http://cache/sign-preparation-activities/"
  end

  match "/sign-signing-activities/*path", @any do
    Proxy.forward conn, path, "http://cache/sign-signing-activities/"
  end

  match "/sign-refusal-activities/*path", @any do
    Proxy.forward conn, path, "http://cache/sign-refusal-activities/"
  end

  match "/sign-cancellation-activities/*path", @any do
    Proxy.forward conn, path, "http://cache/sign-cancellation-activities/"
  end

  match "/sign-completion-activities/*path", @any do
    Proxy.forward conn, path, "http://cache/sign-completion-activities/"
  end

  match "/signed-pieces/*path", @any do
    Proxy.forward conn, path, "http://cache/signed-pieces/"
  end

  get "/signing-flows/:signing_flow_id/pieces", @any do
    Proxy.forward conn, [], "http://digital-signing/signing-flows/" <> signing_flow_id <> "/pieces"
  end

  post "/signing-flows/:signing_flow_id/upload-document-to-signinghub", @any do
    Proxy.forward conn, [], "http://digital-signing/signing-flows/" <> signing_flow_id <> "/upload-document-to-signinghub"
  end

  post "/sign-flows/:signing_flow_id/pieces/:piece_id/signers", @any do
    Proxy.forward conn, [], "http://digital-signing/signing-flows/" <> signing_flow_id <> "/pieces/" <> piece_id <> "/signers"
  end

  get "/signing-flows/:signing_flow_id/pieces/:piece_id/signinghub-url", @any do
    Proxy.forward conn, [], "http://digital-signing/signing-flows/" <> signing_flow_id <> "/pieces/" <> piece_id <> "/signinghub-url"
  end

  post "/signing-flows/:signing_flow_id/start", @any do
    Proxy.forward conn, [], "http://digital-signing/signing-flows/" <> signing_flow_id <> "/start"
  end

  get "/mail-folders/*path" do
    Proxy.forward conn, path, "http://cache/mail-folders/"
  end

  match "/emails/*path" do
    Proxy.forward conn, path, "http://cache/emails/"
  end

  get "/recovery-status/*path", @any do
    Proxy.forward conn, [], "http://database:8890/recovery-status/"
  end

  match "/email-notification-settings/*path", @any do
    Proxy.forward conn, path, "http://cache/email-notification-settings/"
  end

  match "/publication-metrics-export-jobs/*path", @any do
    Proxy.forward conn, path, "http://cache/publication-metrics-export-jobs/"
  end

  match "_", %{ last_call: true } do
    send_resp( conn, 404, "Route not found.  See config/dispatcher.ex" )
  end

end
