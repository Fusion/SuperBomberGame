open Sdl;
open GameEngineDefs;


/*
 * An all-purpose function that returns an actor list
 * with the player being updated whichever way we want.
 */
let rec update_player: (actors, actors, reactions, 'a) => (actors, reactions)
= (actors, updated_actors, reactions_list, action) => {
    switch actors {
    | [] => (updated_actors, reactions_list)
    | [head, ...tail] => {
        switch(head.actor_type) {
        | Player => {
            let (new_actor_info, reaction) = head |> action;
            switch(reaction) {
            | Noop => update_player(tail, [new_actor_info, ...updated_actors], reactions_list, action)
            | _ => update_player(tail, [new_actor_info, ...updated_actors], [reaction, ...reactions_list], action)
            }
        }
        | _ => update_player(tail, [head, ...updated_actors], reactions_list, action)
        }
    }};
};


let handle_events: snapshot => snapshot
= world => {

    let react_to_event: actor => (actor, reaction_type)
    = actor => {
        switch(Event.poll_event()) {
        | None => (actor, Noop)
        | Some(ev) => switch(ev) {
            | Event.KeyDown({keycode: Keycode.Q, _})
            | Event.KeyDown({keycode: Keycode.Escape, _})
            | Event.Quit(_) => {
                (actor, End_the_world)
            }
            | Event.KeyDown({keycode: code, _}) => {
                let key = code |> Keycode.to_string;
                /*Printf.printf("Key %s\n%!", key);*/
                switch(key) {
                | "Up" => ({...actor, motion: Moving, orientation: Up}, Noop)
                | "Down" => ({...actor, motion: Moving, orientation: Down}, Noop)
                | "Left" => ({...actor, motion: Moving, orientation: Left}, Noop)
                | "Right" => ({...actor, motion: Moving, orientation: Right}, Noop)
                | "Space" => (actor, Drop_a_bomb(actor.x, actor.y))
                | _ => (actor, Noop)
                }
            }
            | _ => (actor, Noop)
            }
        };
    };

    /*
     * Subtlety: while I will be building an updated actors list from scratch,
     * I will be *adding* to my reactions list; i.e. adding to still unprocessed reactions.
     */
    let (actors, reactions) = update_player(world.actors, [], world.reactions, react_to_event);
    {...world, actors: actors, reactions: reactions}
};