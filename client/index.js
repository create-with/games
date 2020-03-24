import { Elm } from "./src/Main.elm";
import { Howl } from "howler";

const flags = {};
const node = document.getElementById("elm-node");
const app = Elm.Main.init({ node, flags });

app.ports.playSound.subscribe(data => {
  console.log("Playing sound: " + data);

  const soundPath = "./assets/sounds/" + data;
  const sound = new Howl({
    src: [soundPath]
  });

  sound.play();
});
