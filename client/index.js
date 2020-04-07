import { Elm } from "./src/Main.elm";
import { Howl } from "howler";

// Elm Application

const flags = {};
const node = document.getElementById("elm-node");
const app = Elm.Main.init({ node, flags });

// Sounds with Howler

app.ports.playSound.subscribe(data => {
  const soundPath = "./assets/sounds/" + data;
  const sound = new Howl({
    src: [soundPath],
    volume: 0.5
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
