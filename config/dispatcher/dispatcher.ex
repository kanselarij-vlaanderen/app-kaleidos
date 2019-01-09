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

  match "/dossiers/*path" do
    Proxy.forward conn, path, "http://resource/dossiers/"
  end

  match "/domains/*path" do
    Proxy.forward conn, path, "http://resource/domains/"
  end

  match "/themes/*path" do
    Proxy.forward conn, path, "http://resource/themes/"
  end

  match "/responsibilities/*path" do
    Proxy.forward conn, path, "http://resource/responsibilities/"
  end

  match "/sessions/*path" do
    Proxy.forward conn, path, "http://resource/sessions/"
  end

  match _ do
    send_resp( conn, 404, "Route not found.  See config/dispatcher.ex" )
  end

end
