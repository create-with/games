defmodule Games.Repo do
  use Ecto.Repo,
    otp_app: :games,
    adapter: Ecto.Adapters.Postgres
end
