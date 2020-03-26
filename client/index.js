import { Elm } from "./src/Main.elm";
import { Howl } from "howler";

// Elm Application

const flags = {};
const node = document.getElementById("elm-node");
const app = Elm.Main.init({ node, flags });

// Sounds with Howler

app.ports.playSound.subscribe(data => {
  console.log("Playing sound: " + data);

  const soundPath = "./assets/sounds/" + data;
  const sound = new Howl({
    src: [soundPath]
  });

  sound.play();
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
