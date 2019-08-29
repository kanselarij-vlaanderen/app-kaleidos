defmodule Dispatcher do
  use Plug.Router

  def start(_argv) do
    port = 80
    IO.puts "Starting Plug with Cowboy on port #{port}"
    Plug.Adapters.Cowboy.http __MODULE__, [], port: port
    :timer.sleep(:infinity)
  end

  plug Plug.Logger
  plug :match
  plug :dispatch

  match "/agendaitems/search/*path" do
    Proxy.forward conn, path, "http://musearch/agendaitems/search/"
  end

  match "/agendaitems/index/*path" do
    Proxy.forward conn, path, "http://musearch/agendaitems/search/"
  end

  match "/agendaitems/invalidate/*path" do
    Proxy.forward conn, path, "http://musearch/agendaitems/invalidate/"
  end

  match "/cases/search/*path" do
    Proxy.forward conn, path, "http://musearch/cases/search/"
  end

  match "/cases/index/*path" do
    Proxy.forward conn, path, "http://musearch/cases/search/"
  end

  match "/cases/invalidate/*path" do
    Proxy.forward conn, path, "http://musearch/cases/invalidate/"
  end

  match "/casesByDecisionText/search/*path" do
    Proxy.forward conn, path, "http://musearch/casesByDecisionText/search/"
  end

  match "/casesByDecisionText/index/*path" do
    Proxy.forward conn, path, "http://musearch/casesByDecisionText/search/"
  end

  match "/casesByDecisionText/invalidate/*path" do
    Proxy.forward conn, path, "http://musearch/casesByDecisionText/invalidate/"
  end

  match "/oc-agendaitems/search/*path" do
    Proxy.forward conn, path, "http://musearch/oc-agendaitems/search/"
  end

  match "/oc-agendaitems/index/*path" do
    Proxy.forward conn, path, "http://musearch/oc-agendaitems/search/"
  end

  match "/oc-agendaitems/invalidate/*path" do
    Proxy.forward conn, path, "http://musearch/oc-agendaitems/invalidate/"
  end

  match "/musearch/settings/*path" do
    Proxy.forward conn, path, "http://musearch/settings/"
  end

  get "/document-versions/:id/convert" do
    Proxy.forward conn, [], "http://document-conversion/convert-document-versions/" <> id
  end

  match "/agendas/*path" do
    Proxy.forward conn, path, "http://cache/agendas/"
  end
  match "/agendaitems/*path" do
    Proxy.forward conn, path, "http://cache/agendaitems/"
  end
  match "/announcements/*path" do
    Proxy.forward conn, path, "http://cache/announcements/"
  end
  match "/postponeds/*path" do
    Proxy.forward conn, path, "http://cache/postponeds/"
  end
  match "/decisions/*path" do
    Proxy.forward conn, path, "http://cache/decisions/"
  end
  match "/bestuurseenheden/*path" do
    Proxy.forward conn, path, "http://cache/bestuurseenheden/"
  end
  match "/werkingsgebieden/*path" do
    Proxy.forward conn, path, "http://cache/werkingsgebieden/"
  end
  match "/bestuurseenheid-classificatie-codes/*path" do
    Proxy.forward conn, path, "http://cache/bestuurseenheid-classificatie-codes/"
  end
  match "/bestuursorgaan-classificatie-codes/*path" do
    Proxy.forward conn, path, "http://cache/bestuursorgaan-classificatie-codes/"
  end
  match "/meetings/*path" do
    Proxy.forward conn, path, "http://cache/meetings/"
  end
  match "/meeting-records/*path" do
    Proxy.forward conn, path, "http://cache/meeting-records/"
  end
  match "/documents/*path" do
    Proxy.forward conn, path, "http://cache/documents/"
  end

  match "/document-versions/*path" do
    Proxy.forward conn, path, "http://cache/document-versions/"
  end

  match "/document-types/*path" do
    Proxy.forward conn, path, "http://cache/document-types/"
  end

  match "/document-type-codes/*path" do
    Proxy.forward conn, path, "http://cache/document-type-codes/"
  end
  match "/translaterequests/*path" do
    Proxy.forward conn, path, "http://cache/translaterequests/"
  end
  match "/translaterequest-statusses/*path" do
    Proxy.forward conn, path, "http://cache/translaterequest-statusses/"
  end
  match "/media-type-codes/*path" do
    Proxy.forward conn, path, "http://cache/media-type-codes/"
  end
  match "/cases/*path" do
    Proxy.forward conn, path, "http://cache/cases/"
  end

  match "/case-types/*path" do
    Proxy.forward conn, path, "http://cache/case-types/"
  end

  match "/policy-levels/*path" do
    Proxy.forward conn, path, "http://cache/policy-levels/"
  end

  match "/submitters/*path" do
    Proxy.forward conn, path, "http://cache/submitters/"
  end

  match "/subcases/*path" do
    Proxy.forward conn, path, "http://cache/subcases/"
  end
   match "/subcase-types/*path" do
    Proxy.forward conn, path, "http://cache/subcase-types/"
  end
  match "/subcase-phases/*path" do
    Proxy.forward conn, path, "http://cache/subcase-phases/"
  end
  match "/subcase-phase-codes/*path" do
    Proxy.forward conn, path, "http://cache/subcase-phase-codes/"
  end
  match "/access-levels/*path" do
    Proxy.forward conn, path, "http://cache/access-levels/"
  end
  match "/approvals/*path" do
    Proxy.forward conn, path, "http://cache/approvals/"
  end
  match "/consultation-requests/*path" do
    Proxy.forward conn, path, "http://cache/consultation-requests/"
  end
  match "/consultation-types/*path" do
    Proxy.forward conn, path, "http://cache/consultation-types/"
  end
  match "/consultation-responses/*path" do
    Proxy.forward conn, path, "http://cache/consultation-responses/"
  end
  match "/consultation-response-codes/*path" do
    Proxy.forward conn, path, "http://cache/consultation-response-codes/"
  end

  match "/document-states/*path" do
    Proxy.forward conn, path, "http://cache/document-states/"
  end

  match "/file-addresses/*path" do
    Proxy.forward conn, path, "http://cache/file-addresses/"
  end

  match "/births/*path" do
    Proxy.forward conn, path, "http://cache/births/"
  end
  match "/mandates/*path" do
    Proxy.forward conn, path, "http://cache/mandates/"
  end
  match "/government-functions/*path" do
    Proxy.forward conn, path, "http://cache/government-functions/"
  end
  match "/government-bodies/*path" do
    Proxy.forward conn, path, "http://resource/government-bodies/"
  end
  match "/mandatees/*path" do
    Proxy.forward conn, path, "http://cache/mandatees/"
  end
  match "/mandatee-states/*path" do
    Proxy.forward conn, path, "http://cache/mandatee-states/"
  end
  match "/government-fields/*path" do
    Proxy.forward conn, path, "http://cache/government-fields/"
  end
  match "/government-domains/*path" do
    Proxy.forward conn, path, "http://cache/government-domains/"
  end
  match "/ise-codes/*path" do
    Proxy.forward conn, path, "http://cache/ise-codes/"
  end
  match "/responsibilities/*path" do
    Proxy.forward conn, path, "http://cache/responsibilities/"
  end
  match "/people/*path" do
    Proxy.forward conn, path, "http://cache/people/"
  end
  match "/genders/*path" do
    Proxy.forward conn, path, "http://cache/genders/"
  end
  match "/identifications/*path" do
    Proxy.forward conn, path, "http://cache/identifications/"
  end
  match "/time-periods/*path" do
    Proxy.forward conn, path, "http://cache/time-periods/"
  end

  match "/sites/*path" do
    Proxy.forward conn, path, "http://cache/sites/"
  end
  match "/contact-points/*path" do
    Proxy.forward conn, path, "http://cache/contact-points/"
  end
  match "/posts/*path" do
    Proxy.forward conn, path, "http://cache/posts/"
  end
  match "/roles/*path" do
    Proxy.forward conn, path, "http://cache/roles/"
  end
  match "/organizations/*path" do
    Proxy.forward conn, path, "http://cache/organizations/"
  end

  match "/publications/*path" do
    Proxy.forward conn, path, "http://cache/publications/"
  end
  match "/publication-states/*path" do
    Proxy.forward conn, path, "http://cache/publication-states/"
  end
  match "/publication-state-codes/*path" do
    Proxy.forward conn, path, "http://cache/publication-state-codes/"
  end
  match "/remarks/*path" do
    Proxy.forward conn, path, "http://cache/remarks/"
  end
  match "/newsletter-infos/*path" do
    Proxy.forward conn, path, "http://cache/newsletter-infos/"
  end
  match "/themes/*path" do
    Proxy.forward conn, path, "http://cache/themes/"
  end

  match "/users/*path" do
    Proxy.forward conn, path, "http://cache/users/"
  end

  match "/accounts/*path" do
    Proxy.forward conn, path, "http://cache/accounts/"
  end

  match "/agenda-sort/*path" do
    Proxy.forward conn, path, "http://agenda-sort-service/"
  end

  match "/custom-subcases/*path" do
    Proxy.forward conn, path, "http://custom-subcases-service/"
  end

  match "/session-service/*path" do
    Proxy.forward conn, path, "http://session-number-service/"
  end

  match "/file-bundling-service/*path" do
    Proxy.forward conn, path, "http://file-bundling-service/"
  end

   match "/document-grouping-service/*path" do
    Proxy.forward conn, path, "http://document-grouping-service/"
  end

  match "/agenda-approve/*path" do
    Proxy.forward conn, path, "http://agenda-approve-service/"
  end

  match "/account-groups/*path" do
    Proxy.forward conn, path, "http://cache/account-groups/"
  end

  match "/signatures/*path" do
    Proxy.forward conn, path, "http://cache/signatures/"
  end

  get "/files/:id/download" do
    Proxy.forward conn, [], "http://file/files/" <> id <> "/download"
  end

  post "/files/*path" do
    Proxy.forward conn, path, "http://file/files/"
  end

  delete "/files/*path" do
    Proxy.forward conn, path, "http://file/files/"
  end

  match "/files/*path" do
    Proxy.forward conn, path, "http://resource/files/"
  end

  match "/mock/sessions/*path" do
    Proxy.forward conn, path, "http://mocklogin/sessions/"
  end
  match "/sessions/*path" do
    Proxy.forward conn, path, "http://login/sessions/"
  end

  match "/alerts/*path" do
    Proxy.forward conn, path, "http://cache/alerts/"
  end

  match "/minister-jurisdiction-service/*path" do
    Proxy.forward conn, path, "http://minister-jurisdiction-service/"
  end

  match "/alert-types/*path" do
    Proxy.forward conn, path, "http://cache/alert-types"
  end

  match "/shortcuts/*path" do
    Proxy.forward conn, path, "http://cache/shortcuts/"
  end

  match "/newsletter/*path" do
    Proxy.forward conn, path, "http://newsletter-service/"
  end

  get "/files/:id/download" do
    Proxy.forward conn, [], "http://range-file/files/" <> id <> "/download"
  end

  match "/oc-meetings/*path" do
    Proxy.forward conn, path, "http://resource/oc-meetings/"
  end

  match "/oc-agendaitems/*path" do
    Proxy.forward conn, path, "http://resource/oc-agendaitems/"
  end

  match "/oc-cases/*path" do
    Proxy.forward conn, path, "http://resource/oc-cases/"
  end

  match "/mail-campaigns/*path" do
    Proxy.forward conn, path, "http://resource/mail-campaigns/"
  end

  match _ do
    send_resp( conn, 404, "Route not found.  See config/dispatcher.ex" )
  end

end
