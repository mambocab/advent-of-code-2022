from dataclasses import dataclass
from enum import Enum
from functools import cache
import operator as op
from itertools import accumulate, chain, tee
from typing import Iterable, Optional, Tuple


class Direction(Enum):
    UP = U = "U"
    DOWN = D = "D"
    LEFT = L = "L"
    RIGHT = R = "R"


@cache
@dataclass(frozen=True)
class Point:
    x: int = 0
    y: int = 0

    @property
    def up(self) -> "Point":
        return Point(x=self.x, y=self.y + 1)

    @property
    def down(self) -> "Point":
        return Point(x=self.x, y=self.y - 1)

    @property
    def right(self) -> "Point":
        return Point(x=self.x - 1, y=self.y)

    @property
    def left(self) -> "Point":
        return Point(x=self.x + 1, y=self.y)

    def move(self, direction: str) -> "Point":
        match Direction(direction):
            case Direction.U:
                return self.up
            case Direction.D:
                return self.down
            case Direction.L:
                return self.left
            case Direction.R:
                return self.right
        raise ValueError("invalid direction", direction)

    def drag(self, other: "Point") -> "Point":
        """If self moves, and other is behind it, what position will other end up in?"""
        if self.touching(other):
            return other
        if self.x == other.x or self.y == other.y:
            if n := next(filter(self.touching, other.cardinal_adjacents)):
                return n
            raise ValueError("could not find new tail")
        if n := next(filter(self.touching, other.diagonal_adjacents)):
            return n
        raise ValueError("could not find new tail")

    def touching(self, other: "Point") -> bool:
        return abs(self.x - other.x) <= 1 and abs(self.y - other.y) <= 1

    @property
    def cardinal_adjacents(self) -> Iterable["Point"]:
        yield self.up
        yield self.right
        yield self.down
        yield self.left

    @property
    def diagonal_adjacents(self) -> Iterable["Point"]:
        yield self.up.right
        yield self.right.down
        yield self.down.left
        yield self.left.up


@dataclass
class State:
    knots: Tuple[Point] = None

    def __init__(
        self, length: Optional[int] = None, knots: Optional[Tuple[Point]] = None
    ):
        if length is not None and knots is not None:
            raise ValueError("length and knots arguments are mutually exclusive")
        if knots is not None:
            self.knots = knots
        else:
            self.knots = tuple(Point() for _ in range(length))

    def move(self, direction: str) -> "State":
        if not self.knots:
            return State(0)
        h = self.knots[0].move(direction)
        return State(
            knots=tuple(accumulate(self.knots[1:], func=Point.drag, initial=h))
        )

    @property
    def tail(self):
        return self.knots[-1]


# fh = open("input.txt")
fh = open("input.txt")

split_lines = map(op.methodcaller("split"), map(op.methodcaller("strip"), fh))
flattened_cmds = chain.from_iterable(map(lambda s: s[0] * int(s[1]), split_lines))
p1_cmds, p2_cmds = tee(flattened_cmds)

part1_states = accumulate(p1_cmds, func=State.move, initial=State(2))
print("part 1:", len(set(map(op.attrgetter("tail"), part1_states))))
part2_states = accumulate(p2_cmds, func=State.move, initial=State(10))
print("part 2:", len(set(map(op.attrgetter("tail"), part2_states))))
