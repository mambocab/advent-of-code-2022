from ast import Tuple
from dataclasses import dataclass, field
from enum import Enum, auto
from functools import lru_cache
from itertools import tee
from time import sleep
from typing import Iterator, List, Optional


class CellType(Enum):
    """Represents the type of a cell in the grid"""

    NORMAL = auto()
    START = auto()
    END = auto()

    @classmethod
    def from_string(cls, s: str) -> "CellType":
        if len(s) > 1:
            raise ValueError("string must be one character long")

        match s:
            case "S":
                return cls.START
            case "E":
                return cls.END
        return cls.NORMAL


class Direction(Enum):
    """Represents a direction in the grid"""

    RIGHT = auto()
    DOWN = auto()
    LEFT = auto()
    UP = auto()


_lowercase_a = ord("a")


@dataclass(frozen=True)
class Cell:
    elevation: int
    type: CellType = CellType.NORMAL

    @classmethod
    def from_string(cls, s: str) -> "Cell":
        if len(s) > 1:
            raise ValueError("string must be one character long")

        match s:
            case "S":
                elevation = 0
            case "E":
                elevation = ord("z") - _lowercase_a
            case _:
                if not s.islower():
                    raise ValueError("invalid character")
                elevation = ord(s) - _lowercase_a

        return Cell(
            elevation=elevation,
            type=CellType.from_string(s),
        )

    def __str__(self) -> str:
        if self.type == CellType.START:
            return "S"
        elif self.type == CellType.END:
            return "E"
        return chr(self.elevation + _lowercase_a)


Point = tuple[int, int]
Solution = tuple[Point, ...]


def solution_to_string(solution: Solution) -> str:
    max_x, max_y = 0, 0
    for x, y in solution:
        max_x = max(max_x, x)
        max_y = max(max_y, y)

    grid_builder: list[list[str]] = [["."] * (max_x + 1) for _ in range(max_y + 1)]

    p, next_p = tee(solution)
    next(next_p)
    for (x, y), (next_x, next_y) in zip(p, next_p):
        match (next_x - x, next_y - y):
            case (1, 0):
                grid_builder[y][x] = ">"
            case (-1, 0):
                grid_builder[y][x] = "<"
            case (0, 1):
                grid_builder[y][x] = "v"
            case (0, -1):
                grid_builder[y][x] = "^"
            case _:
                ValueError("invalid solution")
    grid_builder[next_y][next_x] = "E"

    return "\n".join("".join(row) for row in grid_builder)


@dataclass
class Grid:
    cells: tuple[tuple[Cell, ...], ...]
    width: int
    height: int
    start: Point
    end: Point

    _iterations: int = 0

    @classmethod
    def from_string(cls, s: str) -> "Grid":
        lines = s.splitlines()
        height = len(lines)
        width = len(lines[0])
        print(f"width, height: {width}, {height}")

        grid_builder: list[tuple[Cell, ...]] = []
        start: Optional[Point] = None
        end: Optional[Point] = None

        line: str
        for line_idx, line in enumerate(lines):
            if line and (len(line) != width):
                raise ValueError("all lines must be the same length")

            line_builder = []

            for cell_idx, c in enumerate(line):
                if not c.isalpha():
                    raise ValueError("all characters must be alphabetic")

                cell = Cell.from_string(c)
                line_builder.append(cell)
                if cell.type == CellType.START:
                    start = (cell_idx, line_idx)
                elif cell.type == CellType.END:
                    end = (cell_idx, line_idx)

            grid_builder.append(tuple(line_builder))

        if start is None:
            raise ValueError("no start cell found")
        if end is None:
            raise ValueError("no end cell found")

        return cls(
            cells=tuple(grid_builder),
            width=width,
            height=height,
            start=start,
            end=end,
        )

    def __str__(self) -> str:
        return "\n".join("".join(str(c) for c in row) for row in self.cells)

    def __hash__(self) -> int:
        return hash(self.cells)

    @lru_cache
    def adjacents(self, p: Point) -> frozenset[Point]:
        x, y = p

        rv = frozenset(
            (x + dx, y + dy)
            for dx, dy in ((0, 1), (1, 0), (0, -1), (-1, 0))
            if (0 <= x + dx < self.width) and (0 <= y + dy < self.height)
        )
        return rv

    @lru_cache
    def valid_moves(self, p: Point) -> frozenset[Point]:
        return frozenset(self._valid_moves_generator(p))

    def _valid_moves_generator(self, p: Point) -> Iterator[Point]:
        px, py = p
        max_elevation = self.cells[py][px].elevation + 1
        for adj in self.adjacents(p):
            if self.cells[adj[1]][adj[0]].elevation <= max_elevation:
                yield adj

    def solutions(self) -> Iterator[Solution]:
        self._iterations = 0
        yield from self._solutions(paths=((self.start,),))

    def _solutions(self, paths: tuple[Solution, ...]) -> Iterator[Solution]:
        if not paths:
            return

        self._iterations += 1
        if self._iterations % 1 == 0:
            print(f"{self._iterations} iterations; checking {len(paths)} paths")

        new_paths: list[Solution] = []
        for path in paths:
            if path[-1] == self.end:
                yield path
            else:
                new_paths.extend(self._extend_path(path))
        yield from self._solutions(tuple(new_paths))

    def _extend_path(self, path: Solution) -> Iterator[Solution]:
        p = path[-1]
        for move in self.valid_moves(p):
            if move not in path:
                yield path + (move,)


for filename in ("example.txt", "input.txt"):
    # for now just the one
    if filename.startswith("example"):
        continue

    print(filename)
    print()

    with open(filename) as fh:
        grid = Grid.from_string(fh.read())

    print(str(grid))

    solution = next(grid.solutions())
    print(len(solution) - 1, ":", solution)

    print(solution_to_string(solution))
