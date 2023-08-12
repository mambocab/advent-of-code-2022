input_lines = list(open("input.txt").readlines())
input_grid = "".join(input_lines)
coords = lambda i: complex(*divmod(i, len(input_lines[0])))
height = lambda c: ord(grid.get(c, "Z").replace("S", "a").replace("E", "z"))
source = coords(input_grid.index("E"))
grid = {coords(x): input_grid[x] for x in range(len(input_grid))}

# lines_in = open("example.txt").readlines()
# grid_in = "".join(lines_in)


# def coords(i: int) -> complex:
#     return complex(*divmod(i, len(lines_in[0])))


# def height(c: complex) -> int:
#     return ord(grid.get(c, "Z").replace("S", "a"))


# source = coords(grid_in.index("E"))
# grid = {coords(x): grid_in[x] for x in range(len(grid_in))}

for target in "S", "Sa":
    print("target =", target)
    todo, done = [(source, 0)], {source}

    while todo:
        old, dist = todo.pop(0)
        if grid[old] in target:
            print(dist)
            break
        for new in (old + 1, old - 1, old + 1j, old - 1j):
            if new not in done and height(old) - height(new) <= 1:
                todo.append((new, dist + 1))
                done.add(new)
