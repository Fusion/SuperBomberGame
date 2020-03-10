type orientation = Down | Up | Left | Right;

type actor = {
    frame: int,
    orientation: orientation,
    moving: bool,
    x: int,
    y: int
};

type snapshot = {
    player: actor
};