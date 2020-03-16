type orientation = Down | Up | Left | Right;
type entropy_type = Worsening | End_of_the_world;
type actor_type  = Available | Player | Bomb;
type reaction_type = Noop | Drop_a_bomb(int, int) | End_the_world;
type motion = Moving | NotMoving;

/*
 * Note to self:
 * Leave polymorphic variants out of it!
 * For this use case, they will require too much boilerplate.
 */
type actor = {
    actor_type: actor_type,
    frame: int,
    orientation: orientation,
    motion: motion,
    x: int,
    y: int,
    bomb_count: int
};

type actors = list(actor);
type reactions = list(reaction_type);

type snapshot = {
    actors: actors,
    reactions: reactions,
    entropy: entropy_type
};
