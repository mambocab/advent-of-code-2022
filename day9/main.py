from dataclasses import dataclass
from enum import Enum
from functools import cache
import operator as op
from itertools import accumulate, chain
from typing import Dict, Iterable, Tuple


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

    @property
    def adjacents(self) -> Iterable["Point"]:
        yield self.up
        yield self.up.right
        yield self.right
        yield self.right.down
        yield self.down
        yield self.down.left
        yield self.left
        yield self.left.up


@dataclass(frozen=True)
class State:
    head: Point = Point(0, 0)
    tail: Point = Point(0, 0)

    def move(self, direction: str) -> "State":
        t = self.tail
        h = self.head.move(direction=direction)
        if h.touching(t):
            return State(head=h, tail=t)
        if h.x == t.x or h.y == t.y:
            if new_tail := next(filter(h.touching, t.cardinal_adjacents)):
                return State(head=h, tail=new_tail)
            raise ValueError("could not find new tail")
        if new_tail := next(filter(h.touching, t.diagonal_adjacents)):
            return State(head=h, tail=new_tail)
        raise ValueError("could not find new tail")


fh = open("input.txt")

split_lines = map(op.methodcaller("split"), map(op.methodcaller("strip"), fh))
flattened_cmds = chain.from_iterable(map(lambda s: s[0] * int(s[1]), split_lines))
states = accumulate(flattened_cmds, func=State.move, initial=State())
print("part 1:", len(set(map(op.attrgetter("tail"), states))))
