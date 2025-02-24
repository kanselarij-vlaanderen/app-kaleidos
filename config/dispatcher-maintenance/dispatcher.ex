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


  ## Fallback

  match "/*_path", @not_found do
    Proxy.forward conn, [], "http://frontend/index.html"
  end

 end
