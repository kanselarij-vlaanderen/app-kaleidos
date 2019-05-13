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

  match "/agendas/*path" do
    Proxy.forward conn, path, "http://resource/agendas/"
  end
  match "/agendaitems/*path" do
    Proxy.forward conn, path, "http://resource/agendaitems/"
  end
  match "/announcements/*path" do
    Proxy.forward conn, path, "http://resource/announcements/"
  end
  match "/postponeds/*path" do
    Proxy.forward conn, path, "http://resource/postponeds/"
  end
  match "/decisions/*path" do
    Proxy.forward conn, path, "http://resource/decisions/"
  end
  match "/bestuurseenheden/*path" do
    Proxy.forward conn, path, "http://resource/bestuurseenheden/"
  end
  match "/werkingsgebieden/*path" do
    Proxy.forward conn, path, "http://resource/werkingsgebieden/"
  end
  match "/bestuurseenheid-classificatie-codes/*path" do
    Proxy.forward conn, path, "http://resource/bestuurseenheid-classificatie-codes/"
  end
  match "/bestuursorgaan-classificatie-codes/*path" do
    Proxy.forward conn, path, "http://resource/bestuursorgaan-classificatie-codes/"
  end
  match "/meetings/*path" do
    Proxy.forward conn, path, "http://resource/meetings/"
  end
  match "/meeting-records/*path" do
    Proxy.forward conn, path, "http://resource/meeting-records/"
  end
  match "/documents/*path" do
    Proxy.forward conn, path, "http://resource/documents/"
  end

  match "/document-versions/*path" do
    Proxy.forward conn, path, "http://resource/document-versions/"
  end

  match "/document-types/*path" do
    Proxy.forward conn, path, "http://resource/document-types/"
  end

  match "/document-type-codes/*path" do
    Proxy.forward conn, path, "http://resource/document-type-codes/"
  end
  match "/translaterequests/*path" do
    Proxy.forward conn, path, "http://resource/translaterequests/"
  end
  match "/translaterequest-statusses/*path" do
    Proxy.forward conn, path, "http://resource/translaterequest-statusses/"
  end
  match "/media-type-codes/*path" do
    Proxy.forward conn, path, "http://resource/media-type-codes/"
  end
  match "/cases/*path" do
    Proxy.forward conn, path, "http://resource/cases/"
  end
  match "/case-types/*path" do
    Proxy.forward conn, path, "http://resource/case-types/"
  end
  match "/subcases/*path" do
    Proxy.forward conn, path, "http://resource/subcases/"
  end
   match "/subcase-types/*path" do
    Proxy.forward conn, path, "http://resource/subcase-types/"
  end
  match "/subcase-phases/*path" do
    Proxy.forward conn, path, "http://resource/subcase-phases/"
  end
  match "/subcase-phase-codes/*path" do
    Proxy.forward conn, path, "http://resource/subcase-phase-codes/"
  end
  match "/confidentialities/*path" do
    Proxy.forward conn, path, "http://resource/confidentialities/"
  end
  match "/approvals/*path" do
    Proxy.forward conn, path, "http://resource/approvals/"
  end
  match "/consultation-requests/*path" do
    Proxy.forward conn, path, "http://resource/consultation-requests/"
  end
  match "/consultation-types/*path" do
    Proxy.forward conn, path, "http://resource/consultation-types/"
  end
  match "/consultation-responses/*path" do
    Proxy.forward conn, path, "http://resource/consultation-responses/"
  end
  match "/consultation-response-codes/*path" do
    Proxy.forward conn, path, "http://resource/consultation-response-codes/"
  end

  match "/document-states/*path" do
    Proxy.forward conn, path, "http://resource/document-states/"
  end

  match "/file-addresses/*path" do
    Proxy.forward conn, path, "http://resource/file-addresses/"
  end

  match "/births/*path" do
    Proxy.forward conn, path, "http://resource/births/"
  end
  match "/mandates/*path" do
    Proxy.forward conn, path, "http://resource/mandates/"
  end
  match "/government-functions/*path" do
    Proxy.forward conn, path, "http://resource/government-functions/"
  end
  match "/mandatees/*path" do
    Proxy.forward conn, path, "http://resource/mandatees/"
  end
  match "/mandatee-states/*path" do
    Proxy.forward conn, path, "http://resource/mandatee-states/"
  end
  match "/government-domains/*path" do
    Proxy.forward conn, path, "http://resource/government-domains/"
  end
  match "/responsibilities/*path" do
    Proxy.forward conn, path, "http://resource/responsibilities/"
  end
  match "/people/*path" do
    Proxy.forward conn, path, "http://resource/people/"
  end
  match "/genders/*path" do
    Proxy.forward conn, path, "http://resource/genders/"
  end
  match "/identifications/*path" do
    Proxy.forward conn, path, "http://resource/identifications/"
  end
  match "/time-periods/*path" do
    Proxy.forward conn, path, "http://resource/time-periods/"
  end

  match "/sites/*path" do
    Proxy.forward conn, path, "http://resource/sites/"
  end
  match "/contact-points/*path" do
    Proxy.forward conn, path, "http://resource/contact-points/"
  end
  match "/posts/*path" do
    Proxy.forward conn, path, "http://resource/posts/"
  end
  match "/roles/*path" do
    Proxy.forward conn, path, "http://resource/roles/"
  end
  match "/organizations/*path" do
    Proxy.forward conn, path, "http://resource/organizations/"
  end

  match "/publications/*path" do
    Proxy.forward conn, path, "http://resource/publications/"
  end
  match "/publication-states/*path" do
    Proxy.forward conn, path, "http://resource/publication-states/"
  end
  match "/publication-state-codes/*path" do
    Proxy.forward conn, path, "http://resource/publication-state-codes/"
  end
  match "/remarks/*path" do
    Proxy.forward conn, path, "http://resource/remarks/"
  end
  match "/newsletter-infos/*path" do
    Proxy.forward conn, path, "http://resource/newsletter-infos/"
  end
  match "/themes/*path" do
    Proxy.forward conn, path, "http://resource/themes/"
  end

  match "/users/*path" do
    Proxy.forward conn, path, "http://resource/users/"
  end

match "/accounts/*path" do
    Proxy.forward conn, path, "http://resource/accounts/"
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

  match "/agenda-approve/*path" do
    Proxy.forward conn, path, "http://agenda-approve-service/"
  end

  match "/account-groups/*path" do
    Proxy.forward conn, path, "http://resource/account-groups/"
  end

  match "/files/*path" do
    Proxy.forward conn, path, "http://file/files/"
  end

  match "/mock/sessions/*path" do
    Proxy.forward conn, path, "http://mocklogin/sessions/"
  end
  match "/sessions/*path" do
    Proxy.forward conn, path, "http://login/sessions/"
  end

  match "/alerts/*path" do
    Proxy.forward conn, path, "http://resource/alerts/"
  end

  match "/alert-types/*path" do
    Proxy.forward conn, path, "http://resource/alert-types"
  end

  match "/close-meeting/*path" do
    Proxy.forward conn, path, "http://close-meeting-service/"
  end

  match "/migrate/*path" do
    Proxy.forward conn, path, "http://data-migration-service/"
  end

  match _ do
    send_resp( conn, 404, "Route not found.  See config/dispatcher.ex" )
  end

end
