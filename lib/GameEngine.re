open Sdl;
open Sbgc;
open World;
open Time_now;

/*
 For a smooth experience, we must make sure that we display a frame within our tick budget.
 (~17s @60/Hz)
*/
let frame_budget = 1000.0 /. 10.0;

let load_pic: (Render.t, string) => Texture.t
= (renderer, path) => {
    let surface = Surface.load_bmp(~filename = path);

    /* Transparency */
    let pixel_format = surface |> Surface.get_pixelformat_t |> Pixel.alloc_format;
    let pixel = Pixel.map_RGB(pixel_format, ~rgb = (255, 255, 255));
    Pixel.free_format(pixel_format);
    Surface.set_color_key(surface, ~enable = true, ~key = pixel);

    let pic = Texture.create_from_surface(renderer, surface);
    Surface.free(surface);
    pic
};

let timestamp: unit => int
= () => {
    (nanoseconds_since_unix_epoch() |> Base.Int63.to_int_trunc) / 1000000
}

let init() = {
    Sdl.init([`VIDEO]);
    let (window, renderer) =
        Render.create_window_and_renderer(~width = canvas_width, ~height = canvas_height, ~flags=[]);
    let world:snapshot = {
        player: {
            frame: 0,
            moving: false,
            orientation: Down,
            x: 1,
            y: 1
        }
    };
    let assets:assets = {
        block: load_pic(renderer, "assets/block.bmp"),
        block2: load_pic(renderer, "assets/block2.bmp"),
        box: load_pic(renderer, "assets/box.bmp"),
        tnt: load_pic(renderer, "assets/tnt.bmp"),
        player: load_pic(renderer, "assets/Bman.bmp")
    };

    ({window, renderer, assets}, world)
};

let finalize() = {
    Timer.delay(~ms = 1000);
    Sdl.quit();
    exit(0);
};

/* Testing grounds -- should be unused in production */
let display_tests: (canvas_type, snapshot) => unit
= (canvas, _) => {
    let rect = Rect.make4(~x = 1, ~y = 1, ~w = 600, ~h = 600);
    canvas.renderer |> Render.set_draw_color(~rgb = canvas_rect_color, ~a = canvas_alpha);
    Render.fill_rect(canvas.renderer, rect);
};

let display_level: (canvas_type, snapshot) => unit
= (canvas, _) => {
    let rect = Rect.make4(~x = 1, ~y = 1, ~w = canvas_width, ~h = canvas_height);
    canvas.renderer |> Render.set_draw_color(~rgb = canvas_rect_color, ~a = canvas_alpha);
    Render.fill_rect(canvas.renderer, rect);

    let tex_rect = Rect.make4(~x=0, ~y=0, ~w=32, ~h=32);

    for (y in 0 to stage_height) {
        for (x in 0 to stage_width) {
            switch(stage_level[y][x]) {
            | 1 => {
                let canvas_rect = Rect.make4(~x=(x * stage_pic_size), ~y=(y * stage_pic_size), ~w=stage_pic_size, ~h=stage_pic_size);
                Render.copy(canvas.renderer, ~texture = canvas.assets.block2, ~src_rect = tex_rect, ~dst_rect = canvas_rect, ()); }
            | 2 => {
                let canvas_rect = Rect.make4(~x=(x * stage_pic_size), ~y=(y * stage_pic_size), ~w=stage_pic_size, ~h=stage_pic_size);
                Render.copy(canvas.renderer, ~texture = canvas.assets.box, ~src_rect = tex_rect, ~dst_rect = canvas_rect, ()); }
            | 3 => {
                let canvas_rect = Rect.make4(~x=(x * stage_pic_size), ~y=(y * stage_pic_size), ~w=stage_pic_size, ~h=stage_pic_size);
                Render.copy(canvas.renderer, ~texture = canvas.assets.tnt, ~src_rect = tex_rect, ~dst_rect = canvas_rect, ()); }
            | 4 => {
                let canvas_rect = Rect.make4(~x=(x * stage_pic_size), ~y=(y * stage_pic_size), ~w=stage_pic_size, ~h=stage_pic_size);
                Render.copy(canvas.renderer, ~texture = canvas.assets.block, ~src_rect = tex_rect, ~dst_rect = canvas_rect, ()); }
            | _ => ()
            };
        }
    }
};

let display_player: (canvas_type, snapshot) => unit
= (canvas, world) => {

    let actual_display: (int, int) => unit
    = (frame_idx, direction_idx) => {
        /*Printf.printf("Frame: %d\n%!", frame_idx);*/
        let tex_rect = Rect.make4(~x=direction_idx*stage_pic_size, ~y=frame_idx*stage_pic_size*2, ~w=64, ~h=128);
        let canvas_rect = Rect.make4(~x=(world.player.x * stage_pic_size), ~y=(world.player.y * stage_pic_size), ~w=stage_pic_size, ~h=stage_pic_size);
        Render.copy(canvas.renderer, ~texture = canvas.assets.player, ~src_rect = tex_rect, ~dst_rect = canvas_rect, ());
    };

    switch(world.player.orientation) {
    | Down => actual_display(world.player.frame, 0)
    | Up => actual_display(world.player.frame, 1)
    | Left => actual_display(world.player.frame, 2)
    | Right => actual_display(world.player.frame, 3)
    };
};

let display: (canvas_type, snapshot) => unit
= (canvas, world) => {
    /*Printf.printf("%s\n%!", "-display-");*/
    canvas.renderer |> Render.set_draw_color(~rgb = canvas_bg_color, ~a = canvas_alpha);
    canvas.renderer |> Render.clear;
    /* canvas |> display_tests; */
    display_level(canvas, world);
    display_player(canvas, world);
    canvas.renderer |> Render.render_present;
};

let move_arrow_of_time: snapshot => snapshot
= world => {
    /*Printf.printf("%s\n%!", "--arrow->");*/

    /* Yes, we update frames in this function rather than when displaying
       as it is ok to lose frames rather than trying to catch up visually */
    let inc_frame: (int) => int
    = frame_idx => {
        switch(frame_idx) {
        | 7 => 0
        | n => n + 1
        }
    };

    /* Bumping into any obstacle? */
    let try_moving: (snapshot, int, int) => (bool, int, int)
    = (world, horizontal, vertical) => {
        let new_x = world.player.x + horizontal;
        let new_y = world.player.y + vertical;
        switch(stage_level[new_y][new_x]) {
        | 0 => (true, new_x, new_y)
        | _ => (false, world.player.x, world.player.y)
        }
    }

    switch(world.player.moving, world.player.orientation) {
    | (false, _) => world
    | (true, Down) => {
        let (still_moving, x,y) = try_moving(world, 0, 1);
        {player: {...world.player, frame: inc_frame(world.player.frame), moving: still_moving, x: x, y: y}} }
    | (true, Up) => {
        let (still_moving, x,y) = try_moving(world, 0, -1);
        {player: {...world.player, frame: inc_frame(world.player.frame), moving: still_moving, x: x, y: y}} }
    | (true, Left) => {
        let (still_moving, x,y) = try_moving(world, -1, 0);
        {player: {...world.player, frame: inc_frame(world.player.frame), moving: still_moving, x: x, y: y}} }
    | (true, Right) => {
        let (still_moving, x,y) = try_moving(world, 1, 0);
        {player: {...world.player, frame: inc_frame(world.player.frame), moving: still_moving, x: x, y: y}} }
    };
};

let handle_events: snapshot => snapshot
= world => {
    switch(Event.poll_event()) {
    | None => world
    | Some(ev) => switch(ev) {
        | Event.KeyDown({keycode: Keycode.Q, _})
        | Event.KeyDown({keycode: Keycode.Escape, _})
        | Event.Quit(_) => {
            finalize()
        }
        | Event.KeyDown({keycode: code, _}) => {
            let key = code |> Keycode.to_string;
            Printf.printf("Key %s\n%!", key);
            switch(key) {
            | "Up" => {player: {...world.player, moving: true, orientation: Up}}
            | "Down" => {player: {...world.player, moving: true, orientation: Down}}
            | "Left" => {player: {...world.player, moving: true, orientation: Left}}
            | "Right" => {player: {...world.player, moving: true, orientation: Right}}
            | " " => world
            | _ => world
            }
        }
        | _ => world
        }
    };
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

        let rec update_: (snapshot, int) => snapshot
        = (world, iter) => {
            switch(iter) {
            | 0 => world
            | _ => update_(world |> move_arrow_of_time, iter - 1)
            }
        };

        let new_world_after_events = world |> handle_events;

        let now = timestamp();
        let elapsed = (now - last_frame) |> Int.to_float;
        if (elapsed >= frame_budget) {
            if (elapsed >= frame_budget *. 2.0) {
                Printf.printf("Warning! Skipped %f frames.\n%!", (elapsed /. frame_budget));
            };
            let new_world_after_time = update_(new_world_after_events, (elapsed /. frame_budget) |> Float.to_int);
            display(canvas, new_world_after_time);
            run_loop(canvas, new_world_after_time, now);
        } else {
            run_loop(canvas, new_world_after_events, last_frame);
        };
    };

    run_loop(canvas, world, timestamp());
};