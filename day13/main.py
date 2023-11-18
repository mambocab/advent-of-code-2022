import ast
from dataclasses import dataclass
from typing import Optional, Union, cast


Comparable = Union[int, list["Comparable"]]


def ordered(left: Comparable, right: Comparable, indent=0, debug=False) -> Optional[bool]:
    if debug:
        print(f"{'  ' * indent}checking <{left}> < <{right}>")
    if isinstance(left, list) and isinstance(right, list):
        for lx, rx in zip(left, right):
            print("checking list items", lx, rx)
            if rv := ordered(lx, rx, indent=indent + 1) is not None:
                print("got rv", rv)
                return rv

        if len(left) != len(right):
            return len(left) < len(right)

        return None

    if isinstance(left, int) and isinstance(right, int):
        print("comparing ints", left, right)
        if left == right:
            return None
        got = left < right
        print("got", got)
        return got

    print("asymmetrical")
    if isinstance(left, int):
        return ordered([left], right, indent=indent + 1, debug=debug)

    # isinstance(right, int)
    return ordered(left, [cast(int, right)], indent=indent + 1, debug=debug)


def parse_line(line: str) -> Comparable:
    return ast.literal_eval(line)


@dataclass
class ParsedPair:
    left: Optional[Comparable]
    right: Optional[Comparable]

    @property
    def ready(self) -> bool:
        return self.left is not None and self.right is not None

    def check(self, debug: bool = False) -> bool:
        if self.left is None or self.right is None:
            raise ValueError("Pair is not full")
        if (rv := ordered(self.left, self.right, debug=debug)) is None:
            raise ValueError("Could not determine order")
        return rv

    def add(self, value: Comparable):
        if self.left is None:
            self.left = value
        elif self.right is None:
            self.right = value
        else:
            raise ValueError("Pair is already full")


def test_pair_3():
    assert ParsedPair([9], [[8, 7, 6]]).check(debug=True) is False


if __name__ == "__main__":
    for filename in ("example.txt", "input.txt"):
        print(filename)

        pair: ParsedPair = ParsedPair(None, None)

        pair_index, ordered_index_sum = 1, 0

        for line in open(filename):
            line.strip()

            if pair.ready:
                if pair.check():
                    ordered_index_sum += pair_index
                    print(f"  {pair_index} is ordered")
                    print(pair)
                pair = ParsedPair(None, None)
                pair_index += 1

            if not line.strip():
                continue
            parsed = parse_line(line.strip())

            pair.add(parsed)

        print(ordered_index_sum)
