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

  match "/casesByDecisionText/search/*path", @any do
    Proxy.forward conn, path, "http://musearch/casesByDecisionText/search/"
  end

  match "/casesByDecisionText/index/*path", @any do
    Proxy.forward conn, path, "http://musearch/casesByDecisionText/search/"
  end

  match "/casesByDecisionText/invalidate/*path", @any do
    Proxy.forward conn, path, "http://musearch/casesByDecisionText/invalidate/"
  end

  match "/musearch/settings/*path", @any do
    Proxy.forward conn, path, "http://musearch/settings/"
  end

  get "/document-versions/:id/convert", @any do
    Proxy.forward conn, [], "http://document-conversion/convert-document-versions/" <> id
  end

  post "/agendas/:id/agendaitems/documents/files/archive", @any do
    Proxy.forward conn, [], "http://file-bundling-job-creation-service/agendas/" <> id <> "/agendaitems/documents/files/archive"
  end
  match "/agendas/*path", @any do
    Proxy.forward conn, path, "http://cache/agendas/"
  end
  match "/agendaitems/*path", @any do
    Proxy.forward conn, path, "http://cache/agendaitems/"
  end
  match "/announcements/*path", @any do
    Proxy.forward conn, path, "http://cache/announcements/"
  end
  match "/agenda-item-treatments/*path", @any do
    Proxy.forward conn, path, "http://cache/agenda-item-treatments/"
  end
  match "/decisions/*path", @any do
    Proxy.forward conn, path, "http://cache/decisions/"
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
  match "/meeting-records/*path", @any do
    Proxy.forward conn, path, "http://cache/meeting-records/"
  end
  
  match "/documents/*path", @any do # TODO: change over path to "document-containers" once frontend fully migrated
    Proxy.forward conn, path, "http://cache/documents/"
  end
  match "/document-versions/*path", @any do # TODO: change over to "documents" once frontend fully migrated
    Proxy.forward conn, path, "http://cache/document-versions/"
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
  match "/access-levels/*path", @any do
    Proxy.forward conn, path, "http://cache/access-levels/"
  end
  match "/approvals/*path", @any do
    Proxy.forward conn, path, "http://cache/approvals/"
  end
  match "/consultation-responses/*path", @any do
    Proxy.forward conn, path, "http://cache/consultation-responses/"
  end
  match "/consultation-response-codes/*path", @any do
    Proxy.forward conn, path, "http://cache/consultation-response-codes/"
  end

  match "/file-addresses/*path", @any do
    Proxy.forward conn, path, "http://cache/file-addresses/"
  end

  match "/mandatees/*path", @any do
    Proxy.forward conn, path, "http://cache/mandatees/"
  end
  match "/government-fields/*path", @any do
    Proxy.forward conn, path, "http://cache/government-fields/"
  end
  match "/government-domains/*path", @any do
    Proxy.forward conn, path, "http://cache/government-domains/"
  end
  match "/ise-codes/*path", @any do
    Proxy.forward conn, path, "http://cache/ise-codes/"
  end
  match "/responsibilities/*path", @any do
    Proxy.forward conn, path, "http://cache/responsibilities/"
  end
  match "/people/*path", @any do
    Proxy.forward conn, path, "http://cache/people/"
  end

  match "/identifications/*path", @any do
    Proxy.forward conn, path, "http://cache/identifications/"
  end
  match "/time-periods/*path", @any do
    Proxy.forward conn, path, "http://cache/time-periods/"
  end

  get "/organizations/*path", @any do
    Proxy.forward conn, path, "http://cache/organizations/"
  end

  match "/remarks/*path", @any do
    Proxy.forward conn, path, "http://cache/remarks/"
  end
  match "/newsletter-infos/*path", @any do
    Proxy.forward conn, path, "http://cache/newsletter-infos/"
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

  match "/lazy-loading/*path", @any do
    Proxy.forward conn, path, "http://lazy-loading-service/"
  end

  match "/account-groups/*path", @any do
    Proxy.forward conn, path, "http://cache/account-groups/"
  end

  match "/signatures/*path", @any do
    Proxy.forward conn, path, "http://cache/signatures/"
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

  match "/minister-jurisdiction-service/*path", @any do
    Proxy.forward conn, path, "http://minister-jurisdiction-service/"
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

  match "_", %{ last_call: true } do
    send_resp( conn, 404, "Route not found.  See config/dispatcher.ex" )
  end

end
