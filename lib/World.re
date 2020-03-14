open GameEngineDefs;

type orientation = Down | Up | Left | Right;
type actor_type  = None | Player | Bomb;

type actor = {
    actor_type: actor_type,
    frame: int,
    orientation: orientation,
    motion: motion,
    x: int,
    y: int
};

type actors = list(actor);

type snapshot = {
    actors: actors
};