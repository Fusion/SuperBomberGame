open Sbgc;
open Time_now;
open GameEngineDefs;
open GameEvents;
open World;
open GameRender;

/*
 For a smooth experience, we must make sure that we display a frame within our tick budget.
 (~17s @60/Hz)
*/
let frame_budget = 1000.0 /. 10.0;


let timestamp: unit => int
= () => {
    (nanoseconds_since_unix_epoch() |> Base.Int63.to_int_trunc) / 1000000
}


let init() = {
    let world:snapshot = {
        actors: [{
            actor_type: Player,
            frame: 0,
            motion: NotMoving,
            orientation: Down,
            x: 1,
            y: 1,
            bomb_count: 2
        }],
        reactions: [],
        entropy: Worsening
    };

    (init_render(), world)
};

let finalize() = {
    finalize_render();
    exit(0);
};


/*
 * Our game loop:
 * We are going to attempt smooth updates.
 * We will render our stage as often as possible in sync with our desired refresh rate.
 * We will try to keep our view of the world updated in sync with what is being rendered.
 * However, should we miss a few frames, we will still make sure to update our world as if
 * we had rendered these frames. It is better to lose a few frames than to create a jerky reality.
 * The need for this sort of adjustments would not be as obvious in a non-GC'd language.
 * Note that event handling (e.g. key presses) will not be subjected to this frequency management
 * in order to minimize event loss.
 */
let run: (canvas_type, snapshot) => unit
= (canvas, world) => {

    let rec run_loop: (canvas_type, snapshot, int) => unit
    = (canvas, world, last_frame) => {

        let rec update_loop: (snapshot, int) => snapshot
        = (world, iter) => {
            switch(iter) {
            | 0 => world
            | _ => update_loop(world |> move_arrow_of_time, iter - 1)
            }
        };

        let new_world_after_events = world |> handle_events;

        let now = timestamp();
        let elapsed = (now - last_frame) |> Int.to_float;
        if (elapsed >= frame_budget) {
            if (elapsed >= frame_budget *. 2.0) {
                Printf.printf("Warning! Skipped %f frames.\n%!", (elapsed /. frame_budget));
            };
            /* TODO Now that I am keeping reactions around, I need to clean them up as I read them! */
            let new_world_after_time = update_loop(new_world_after_events, (elapsed /. frame_budget) |> Float.to_int);
            if(new_world_after_time.entropy == End_of_the_world) {
                finalize();
            };
            display(canvas, new_world_after_time);
            run_loop(canvas, new_world_after_time, now);
        } else {
            run_loop(canvas, new_world_after_events, last_frame);
        };
    };

    run_loop(canvas, world, timestamp());
};