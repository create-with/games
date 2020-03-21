import { Elm } from "./src/Main.elm";
import { Howl } from "howler";

const flags = {};
const node = document.getElementById("elm-node");
const app = Elm.Main.init({ node, flags });

app.ports.playSound.subscribe(data => {
  console.log("Playing sound: " + data);

  const soundPath = setSoundPath(data);
  const sound = new Howl({src: [soundPath]});

  sound.play();
});

const setSoundPath = (name) => {
  let soundPath = null;
  if (name.includes("beep")) soundPath = "beep.413a14a4.wav"
  if (name.includes("boop")) soundPath = "boop.bc3d69ea.wav"
  return soundPath
}
