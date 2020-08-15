defmodule GamesWeb.PageLiveTest do
  use GamesWeb.ConnCase

  import Phoenix.LiveViewTest

  test "disconnected and connected render", %{conn: conn} do
    {:ok, page_live, disconnected_html} = live(conn, "/")
    assert disconnected_html =~ "div id=\"elm\""
    assert render(page_live) =~ "div id=\"elm\""
  end
end
