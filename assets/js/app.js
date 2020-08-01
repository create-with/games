// We need to import the CSS so that webpack will load it.
// The MiniCssExtractPlugin is used to separate it out into
// its own CSS file.
import "../css/app.scss"

// webpack automatically bundles all modules in your
// entry points. Those entry points can be configured
// in "webpack.config.js".
//
// Import deps with the dep name or local files with a relative path, for example:
//
//     import {Socket} from "phoenix"
//     import socket from "./socket"
//
import "phoenix_html"
import { Socket } from "phoenix"
import NProgress from "nprogress"
import { LiveSocket } from "phoenix_live_view"
import { Elm } from "../elm/src/Main.elm"
import { Howl } from "howler"

let hooks = {}

hooks.elm = {
  mounted() {
    const flags = {}
    const node = document.getElementById("elm")
    const app = Elm.Main.init({ node, flags })

    // Sounds with Howler

    app.ports.playSound.subscribe(data => {
      const soundPath = "/sounds/" + data;
      const sound = new Howl({
        src: [soundPath],
        volume: 0.5
      });

      sound.play();
    });

    app.ports.playMusic.subscribe(data => {
      const soundPath = "/sounds/" + data;
      const sound = new Howl({
        src: [soundPath],
        autoplay: true,
        loop: true,
        volume: 0.5
      });
    });

    // Prevent Default Keyboard Behavior

    const gameKeys = {
      " ": 32,
      "ArrowUp": 38,
      "ArrowDown": 40
    };

    const preventDefaultForGameKeys = (event) => {
      const keys = Object.values(gameKeys)
      if (keys.includes(event.keycode) ||
        keys.includes(event.which))
        event.preventDefault();
    }

    document.documentElement.addEventListener(
      "keydown",
      (event) => preventDefaultForGameKeys(event),
      false
    );

    // DevTools Easter Egg

    const string = `
C
r b
e y
a .
t .
e .
d .

B .
i .
j B
a o
n u
. s
. t
. a
. n
. i
🐰🥚
`

    console.log(string);

  }
}

let csrfToken = document.querySelector("meta[name='csrf-token']").getAttribute("content")
let liveSocket = new LiveSocket("/live", Socket, { params: { _csrf_token: csrfToken }, hooks })

// Show progress bar on live navigation and form submits
window.addEventListener("phx:page-loading-start", info => NProgress.start())
window.addEventListener("phx:page-loading-stop", info => NProgress.done())

// connect if there are any LiveViews on the page
liveSocket.connect()

// expose liveSocket on window for web console debug logs and latency simulation:
// >> liveSocket.enableDebug()
// >> liveSocket.enableLatencySim(1000)  // enabled for duration of browser session
// >> liveSocket.disableLatencySim()
window.liveSocket = liveSocket
