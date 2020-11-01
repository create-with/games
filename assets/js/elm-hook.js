import { Elm } from "../elm/src/Main.elm";
import { Howl, Howler } from "howler";

const ElmHook = {
  mounted() {
    const flags = {}
    const node = document.getElementById("elm")
    const app = Elm.Main.init({ node, flags })

    // Sounds with Howler

    app.ports.playSound.subscribe(data => {
      const soundPath = "/sounds/" + data;
      const sound = new Howl({
        src: [soundPath],
        volume: 0.35
      });

      sound.play();
    });

    app.ports.playMusic.subscribe(data => {
      // data example: { play: true, soundFile: "music.wav" }

      const soundPath = "/sounds/" + data.soundFile;
      const sound = new Howl({
        src: [soundPath],
        loop: true,
        volume: 0.15
      });

      const startPlaying = () => {
        const soundId = sound.play();
        sound.rate(0.95, soundId);
        sound.fade(0, 1, 2000, soundId);
      }

      const stopPlaying = () => {
        Howler.stop()
      }

      if (data.play) { startPlaying() }
      if (!data.play) { stopPlaying() }
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

export default ElmHook;
