open Sbg;
open Sbg.CfrIO;

module Inc: Incremental.S = Incremental.Make({});
open Inc;

/*
 * Let's get started
 */
let () = {
  let x = Var.create(13);
  let y = Var.create(17);
  let z = map2(Var.watch(x), Var.watch(y), ~f=(x, y) => x + y);
  let z_o = observe(z);
  stabilize();
  Var.set(x, 19);
  stabilize();
  assert(Observer.value_exn(z_o) == 36);


  notify("Starting game...");
  let (canvas, world) = GameEngine.init();
  GameEngine.run(canvas, world);
  GameEngine.finalize();
};

// vim: syntax=reason
