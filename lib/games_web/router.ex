defmodule GamesWeb.Router do
  use GamesWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, {GamesWeb.LayoutView, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  if Mix.env() in [:dev, :test] do
    import Phoenix.LiveDashboard.Router

    scope "/" do
      pipe_through :browser
      live_dashboard "/dashboard", metrics: GamesWeb.Telemetry
    end
  end

  scope "/", GamesWeb do
    pipe_through :browser

    live "/beta", BetaLive, :index
    live "/*path", PageLive, :index
  end
end
