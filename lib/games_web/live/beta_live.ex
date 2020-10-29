defmodule GamesWeb.BetaLive do
  use GamesWeb, :live_view

  alias Phoenix.LiveView

  @impl LiveView
  def render(assigns) do
    ~L"""
    <div class="flex justify-center py-10" id="beta" phx-hook="BetaHook">
      <canvas id="webgl-canvas" width="800" height="600"></canvas>
    </div>
    """
  end

  @impl LiveView
  def mount(_params, _session, socket) do
    {:ok, socket}
  end

  @impl LiveView
  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end
end
