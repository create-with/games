defmodule GamesWeb.PageLive do
  use GamesWeb, :live_view

  alias Phoenix.LiveView

  @impl LiveView
  def render(assigns) do
    ~L"""
    <div id="elm" phx-hook="ElmHook"></div>
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
