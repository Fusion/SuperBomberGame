open Sbgc;
open GameEngineDefs;


let move_arrow_of_time: snapshot => snapshot
= world => {

    let new_bomb: (int, int) => actor
    = (x, y) => {
        {
            actor_type: Bomb,
            frame: 0,
            motion: NotMoving,
            orientation: Down,
            x: x,
            y: y,
            bomb_count: 0
        }
    };

    let rec update_from_reactions: snapshot => snapshot
    = world => {
        switch(world.reactions) {
        | [] => world
        | [head, ...tail] => {
            switch head {
            | Drop_a_bomb(x, y) => update_from_reactions({...world, actors: [new_bomb(x,y), ...world.actors], reactions: tail})
            | End_the_world => ({...world, entropy: End_of_the_world})
            | _ => update_from_reactions({...world, reactions: tail})
            };
        }}
    };

    let rec update_actors: (actors, actors, actors) => actors
    = (remaining_actors, updated_actors, all_actors) => {

        let update_actor: (actor, actors) => actor
        = (actor, all_actors) => {

            /* Yes, we update frames in this function rather than when displaying
            as it is ok to lose frames rather than trying to catch up visually */
            let next_frame: (int) => int
            = frame_idx => {
                switch(frame_idx) {
                | 7 => 0
                | n => n + 1
                }
            };

            /* Bumping into any obstacle? */
            let try_moving: (actor, int, int) => (motion, int, int)
            = (actor, horizontal, vertical) => {
                let new_x = actor.x + horizontal;
                let new_y = actor.y + vertical;
                switch(
                    actor.actor_type,
                    stage_level[0][new_y][new_x],
                    !List.exists(item => item.x == new_x && item.y == new_y, all_actors)) {
                | (Player, 0, true) => (Moving, new_x, new_y)
                | _ => (NotMoving, actor.x, actor.y)
                }
            }

            switch(actor.motion, actor.orientation) {
            | (NotMoving, _) => actor
            | (Moving, Down) => {
                let (still_moving, x,y) = try_moving(actor, 0, 1);
                {...actor, frame: next_frame(actor.frame), motion: still_moving, x: x, y: y} }
            | (Moving, Up) => {
                let (still_moving, x,y) = try_moving(actor, 0, -1);
                {...actor, frame: next_frame(actor.frame), motion: still_moving, x: x, y: y} }
            | (Moving, Left) => {
                let (still_moving, x,y) = try_moving(actor, -1, 0);
                {...actor, frame: next_frame(actor.frame), motion: still_moving, x: x, y: y} }
            | (Moving, Right) => {
                let (still_moving, x,y) = try_moving(actor, 1, 0);
                {...actor, frame: next_frame(actor.frame), motion: still_moving, x: x, y: y} }
            };
        };

        switch remaining_actors {
        | [] => updated_actors
        | [head, ...tail] => {
            let updated_actor = update_actor(head, all_actors);
            update_actors(tail, [updated_actor, ...updated_actors], all_actors)
        }};
    };
    
    /* First, let's see what pending events we have */
    let new_world = update_from_reactions(world);
    /* Then, update our actors' states */
    {...new_world, actors: update_actors(new_world.actors, [], new_world.actors)}
};
