from collections import OrderedDict
from dataclasses import dataclass
from functools import partial
import operator
from typing import Callable, Dict, Iterable, List, Optional, Union
import typing


OPERAND_MAP = {
    "+": operator.add,
    "*": operator.mul,
}


@dataclass
class OpSelf:
    operation: Callable[[int, int], int]

    def __call__(self, i: int):
        return self.operation(i, i)


def op_self(operation: Callable[[int, int], int]) -> Callable[[int], int]:
    return OpSelf(operation=operation)


def remove_prefix(prefix: str, s: str) -> str:
    if s.startswith(prefix):
        return s[len(prefix) :]
    raise ValueError(f"{s} does not have prefix {prefix}")


@dataclass
class Monkey:
    name: int
    items: List[int]
    operation: Callable[[int], int]
    test_divisor: int

    inspection_count: int = 0

    # start with int, replace with Monkey after parsing all
    _true_branch_target: int = -1
    _false_branch_target: int = -1
    true_branch_target: Optional["Monkey"] = None
    false_branch_target: Optional["Monkey"] = None

    def take_turn(self) -> None:
        for item in self.items:
            self.inspection_count += 1
            new_item = self.operation(item) // 3

            if not self.true_branch_target or not self.false_branch_target:
                raise ValueError("cannot throw; missing a branch target")
            if new_item % self.test_divisor == 0:
                self.true_branch_target.catch(new_item)
            else:
                self.false_branch_target.catch(new_item)

        self.items = []

    def catch(self, item: int) -> None:
        self.items.append(item)

    @staticmethod
    def parse(monkey_spec: Iterable[str]) -> "Monkey":
        print("asked to parse", monkey_spec)
        if len(monkey_spec := tuple(monkey_spec)) != 6:
            raise ValueError("invalid length for monkey spec", monkey_spec)
        name_line, items_line, op_line, test_line, true_line, false_line = monkey_spec

        return Monkey(
            name=int(remove_prefix("Monkey ", name_line).split(":")[0]),
            items=[int(s) for s in remove_prefix("Starting items: ", items_line).split(", ")],
            operation=Monkey._parse_op_line(op_line),
            test_divisor=int(remove_prefix("Test: divisible by ", test_line)),
            _true_branch_target=int(remove_prefix("If true: throw to monkey ", true_line)),
            _false_branch_target=int(remove_prefix("If false: throw to monkey ", false_line)),
        )

    @staticmethod
    def _parse_op_line(op_line: str) -> Callable[[int], int]:
        op_spec = op_line.split("=")[1].strip().split(" ")
        if len(op_spec) != 3:
            raise ValueError(f"invalid op_spec length: {op_spec}")

        left, operator, right = op_spec
        if (operation := OPERAND_MAP.get(operator)) is None:
            raise ValueError(f"invalid operand: {operator}")

        if left == right == "old":
            return op_self(operation=operation)
        if left == "old" and (other := right) or right == "old" and (other := left):
            return partial(operation, int(other))

        raise ValueError("couldn't make operation from op_spec:", op_line)


def parse(filename: str) -> typing.OrderedDict[int, Monkey]:
    monkeys = OrderedDict()

    with open(filename) as fh:
        accum: List[str] = []
        for line in fh:
            line = line.strip()
            if not line:  # assumes an empty line between monkey defs
                m = Monkey.parse(accum)
                monkeys[m.name] = m
                accum = []
                continue
            accum.append(line)

    print(monkeys)
    for m in monkeys.values():
        m.true_branch_target = monkeys[m._true_branch_target]
        m.false_branch_target = monkeys[m._false_branch_target]

    return monkeys


for filename in ("example.txt", "input.txt"):
    print(filename)
    monkeys = parse(filename=filename)

    for round in range(20):
        for monkey in monkeys.values():
            monkey.take_turn()

    max_count_1 = max_count_2 = 0
    for monkey in monkeys.values():
        c = monkey.inspection_count
        print(f"> Monkey {monkey.name} inspected items {c} times.")
        if c > max_count_2:
            max_count_1, max_count_2 = max_count_2, c

    print(f"Monkey business = {max_count_1} * {max_count_2} = {max_count_1 * max_count_2}")
