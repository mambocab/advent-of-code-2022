from collections import defaultdict
from dataclasses import dataclass, field
from io import TextIOWrapper
from itertools import tee
from typing import Iterable, List, MutableMapping, Optional, Union


@dataclass
class File:
    size: int


@dataclass
class Directory:
    parent: Optional["Directory"]
    name: str
    children: MutableMapping[
        str, Union["Directory", File]
    ] = field(default_factory=dict)

    @property
    def size(self) -> int:
        return sum(c.size for c in self.children.values())

    def find_type_d(self) -> Iterable["Directory"]:
        yield self
        for child in self.children.values():
            if isinstance(child, Directory):
                yield from child.find_type_d()

    def print(self, name='', indent=0):
        print('  ' * indent, '/'+name, self.size)
        for n, c in self.children.items():
            if isinstance(c, File):
                print('  ' * (indent+1), n, c.size)
            else:
                c.print(name=n, indent=indent+1)

        
        

@dataclass
class State:
    root: Directory = None
    pwd: Directory = None

    def handle_command(self, lines: Iterable[str]) -> None:
        if not lines:
            return
        command = lines[0].split(' ')[1:]
        match command[0]:
            case 'cd':
                match dname := command[1]:
                    case '/':
                        self.pwd = self.root
                        return
                    case '..':
                        self.pwd = self.pwd.parent
                        return
                    case _:
                        if not (child := self.pwd.children.get(dname)):
                            self.pwd.children[dname] = child = Directory(
                                parent=self.pwd,
                                name=dname,
                            )
                        self.pwd = child
                        return
            case 'ls':
                for line in lines[1:]:
                    metadata, name = line.split(' ')
                    if metadata == 'dir':
                        if name not in self.pwd.children:
                            print('adding child dir', name, 'to', self.pwd.name)
                            self.pwd.children[name] = Directory(
                                parent=self.pwd, name=name
                            )
                    else:
                        if name not in self.pwd.children:
                            self.pwd.children[name] = File(size=int(metadata))
                            print('added child file', name, 'to', self.pwd.name, 'new size =', self.pwd.size)


root = Directory(parent=None, name='')
state = State(root=root, pwd=root)
cursor, peeker = tee(open('input.txt'))

cmd_with_output: List[str] = []
next_line, EOF = next(peeker), object()
while True:
    if next_line is EOF or next_line.startswith('$ '):
        state.handle_command(cmd_with_output)
        cmd_with_output = []
    if next_line is EOF:
        break
    cmd_with_output.append(next(cursor).strip())
    try:
        next_line = next(peeker)
    except StopIteration:
        next_line = EOF

print('total size:', state.root.size)

part1 = 0
for d in state.root.find_type_d():
    if (s := d.size) < 100000:
        part1 += s

print('part 1:', part1)
