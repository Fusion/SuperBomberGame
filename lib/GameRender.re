open Sdl;
open Sbgc;
open GameEngineDefs;


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


let init_render: unit => canvas_type
= () => {
    Sdl.init([`VIDEO]);
    let (window, renderer) =
        Render.create_window_and_renderer(~width = canvas_width, ~height = canvas_height, ~flags=[]);

    let assets = {
        block: load_pic(renderer, "assets/block.bmp"),
        block2: load_pic(renderer, "assets/block2.bmp"),
        box: load_pic(renderer, "assets/box.bmp"),
        tnt: load_pic(renderer, "assets/tnt.bmp"),
        player: load_pic(renderer, "assets/Bman.bmp"),
        bomb: load_pic(renderer, "assets/Bomb.bmp")
    };

    {window, renderer, assets}
};


let finalize_render: unit => unit
= () => {
    Sdl.quit();
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
            switch stage_level[0][y][x] {
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


let display_player: (canvas_type, actor) => unit
= (canvas, actor) => {

    let actual_display: (int, int) => unit
    = (frame_idx, direction_idx) => {
        /*Printf.printf("Frame: %d\n%!", frame_idx);*/
        let tex_rect = Rect.make4(~x=direction_idx*stage_pic_size, ~y=frame_idx*stage_pic_size*2, ~w=64, ~h=128);
        let canvas_rect = Rect.make4(~x=(actor.x * stage_pic_size), ~y=(actor.y * stage_pic_size), ~w=stage_pic_size, ~h=stage_pic_size);
        Render.copy(canvas.renderer, ~texture = canvas.assets.player, ~src_rect = tex_rect, ~dst_rect = canvas_rect, ());
    };

    switch actor.orientation {
    | Down => actual_display(actor.frame, 0)
    | Up => actual_display(actor.frame, 1)
    | Left => actual_display(actor.frame, 2)
    | Right => actual_display(actor.frame, 3)
    };
};


let display_bomb: (canvas_type, actor) => unit
= (canvas, actor) => {
    let tex_rect = Rect.make4(~x=0, ~y=0, ~w=48, ~h=48);
    let canvas_rect = Rect.make4(~x=(actor.x * stage_pic_size), ~y=(actor.y * stage_pic_size), ~w=stage_pic_size, ~h=stage_pic_size);
    Render.copy(canvas.renderer, ~texture = canvas.assets.bomb, ~src_rect = tex_rect, ~dst_rect = canvas_rect, ());
};



let display: (canvas_type, snapshot) => unit
= (canvas, world) => {

    let rec display_actors: actors => unit
    = actors => {
        switch actors {
        | [] => ()
        | [head, ...tail] => {
            switch head.actor_type {
            | Player => display_player(canvas, head)
            | Bomb => display_bomb(canvas, head)
            | _ => ()
            };
            display_actors(tail);
        }};
    };

    /*Printf.printf("%s\n%!", "-display-");*/
    canvas.renderer |> Render.set_draw_color(~rgb = canvas_bg_color, ~a = canvas_alpha);
    canvas.renderer |> Render.clear;
    /* canvas |> display_tests; */
    display_level(canvas, world);
    display_actors(world.actors);
    canvas.renderer |> Render.render_present;
};