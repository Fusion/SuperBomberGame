open Sbg;
open Sbg.CfrIO;

/*
 * Let's get started
 */
let () = {
  notify("Starting game...");
  let (canvas, world) = GameEngine.init();
  GameEngine.run(canvas, world);
  GameEngine.finalize();
};

// vim: syntax=reason
