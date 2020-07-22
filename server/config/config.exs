# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
use Mix.Config

config :games,
  ecto_repos: [Games.Repo]

# Configures the endpoint
config :games, GamesWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "hN1WyTZ5m4+7KYTHuwhl+s3FHa+8myW3GMCE98BWg+ifqFO2euPvHrQ/URgcT4x0",
  render_errors: [view: GamesWeb.ErrorView, accepts: ~w(html json), layout: false],
  pubsub_server: Games.PubSub,
  live_view: [signing_salt: "yraOBzUF"]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
