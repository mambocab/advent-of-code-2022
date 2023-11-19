import pake
from pake import FileHelper
from typing import Union, Optional
from glob import glob
import ast
from pathlib import Path

pk = pake.init()


ParsedLine = list[list[int] | int]


def parse_line(line: str) -> ParsedLine:
    """Each line happens to be valid Python."""
    try:
        return ast.literal_eval(line)
    except Exception as e:
        raise ValueError("couldn't parse %s", line)


# INPUTS_GLOB = "inputs/*.txt"
INPUTS_GLOB = "inputs/input.txt"
OUTPUT_DIR = "outputs"
OUTPUTS_GLOB = Path(OUTPUT_DIR, "*")


@pk.task(i="pakefile.py", o=glob(INPUTS_GLOB))
def force_rebuild(ctx):
    """Force a rebuild when the pakefile changes."""
    fh = pake.FileHelper(ctx)
    for o in ctx.outputs:
        fh.touch(o)


def _wrapper(f):
    def wrapped(left, right, depth):
        msg = ["  "] * depth
        msg.extend(["- ", "Compare ", str(left), " vs ", str(right)])
        print("".join(msg))
        result, leaf = f(left, right, depth)
        if result is not None:
            if leaf:
                msg = ["  "] * (depth + 1)
                msg.extend(["- ", "Got ", str(result)])
                print("".join(msg))
        return result

    return wrapped


@_wrapper
def _parsed_lines_in_order(
    left: ParsedLine, right: ParsedLine, depth: int
) -> tuple[Optional[bool], bool]:
    if left == right:
        return None, True
    if not left:
        return True, True
    if not right:
        return False, True
    left_head, *left_tail = left
    right_head, *right_tail = right

    lh_type, rh_type = type(left_head), type(right_head)
    if lh_type == int and rh_type == int:
        if left_head != right_head:
            return left_head < right_head, True
        return _parsed_lines_in_order(left_tail, right_tail, depth + 1), False
    if lh_type == list and rh_type == list:
        if (
            list_compare := _parsed_lines_in_order(left_head, right_head, depth + 1)
        ) is not None:
            return list_compare, False
        return _parsed_lines_in_order(left_tail, right_tail, depth + 1), False

    if lh_type == int:
        left_head = [left_head for _ in range(left_head)]
    if rh_type == int:
        right_head = [right_head for _ in range(right_head)]

    if (result := _parsed_lines_in_order(left_head, right_head, depth + 1)) is not None:
        return result, False

    return _parsed_lines_in_order(left_tail, right_tail, depth + 1), False


def parsed_lines_in_order(left, right):
    result = _parsed_lines_in_order(left, right, depth=0)

    if result is None:
        msg = f"called with {left=}, {right=}: got {result=}"
        raise ValueError(msg)
    # print(msg)
    return result


@pk.task(
    force_rebuild,
    i=pake.glob(INPUTS_GLOB),
    o=pake.pattern(Path(OUTPUT_DIR, "check", "%").as_posix()),
)
def check_compare(ctx: pake.TaskContext):
    fh = pake.FileHelper(ctx)
    fh.makedirs(Path(OUTPUT_DIR, "check").as_posix())

    for i, o in ctx.outdated_pairs:
        fh.remove(o)
        left = right = None  # first and second elements of each pair.
        idx = 1
        for line in open(i):
            line = line.strip()
            if not line:
                continue

            if left is None:
                left = line
            elif right is None:
                right = line

            if left and right:
                left, right = parse_line(left), parse_line(right)
                with open(o, "a") as o_writer:
                    print(
                        f"{idx if parsed_lines_in_order(left, right) else 0}",
                        file=o_writer,
                    )
                    print(f"// left ={left}", file=o_writer)
                    print(f"// {right=}", file=o_writer)
                left = right = None
                idx += 1


@pk.task(
    check_compare,
    i=pake.glob(Path(OUTPUT_DIR, "check", "*").as_posix()),
    o=pake.pattern(Path(OUTPUT_DIR, "sum", "%").as_posix()),
)
def sum_(ctx: pake.TaskContext):
    fh = pake.FileHelper(ctx)
    fh.makedirs(Path(OUTPUT_DIR, "sum"))

    for i, o in ctx.outdated_pairs:
        fh.remove(o)
        total = 0
        with open(i) as f:
            raw_idxs = [line.split("//", 1)[0].strip() for line in f]
        idxs = (int(i) for i in raw_idxs if i)
        total += sum(idxs)
        with open(o, "a") as f:
            print(total, file=f)
        print(i, total)


@pk.task
def clean(ctx: pake.TaskContext):
    fh = pake.FileHelper(ctx)
    fh.glob_remove(OUTPUTS_GLOB)


pake.run(pk, tasks=sum_)
