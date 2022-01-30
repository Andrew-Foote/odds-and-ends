from collections import deque
from collections.abc import Iterable, Iterator
from functools import partial
import itertools as it
from operator import getitem
from typing import Protocol, TypeVar
 
T = TypeVar('T')
 
class Tree(Protocol[T]):
    def __call__(self, vertex: T) -> Iterable[T]:
        """Yield each of the vertex's children."""
 
def dfs(tree: Tree[T], start: T) -> Iterator[T]:
    """
    >>> tree = {1: (2, 3), 2: (4,), 3: (5, 6), 4: (7,), 5: (), 6: (8,), 7: (), 8: ()}
    >>> list(dfs(partial(getitem, tree), 1))
    [1, 3, 6, 8, 5, 2, 4, 7]
    """
    stack = deque([start])
     
    while stack:
        vertex = stack.pop()
        yield vertex
        stack.extend(tree(vertex))
         
def bfs(tree: Tree[T], start: T) -> Iterator[T]:
    """
    >>> tree = {1: (2, 3), 2: (4,), 3: (5, 6), 4: (7,), 5: (), 6: (8,), 7: (), 8: ()}
    >>> list(bfs(partial(getitem, tree), 1))
    [1, 2, 3, 4, 5, 6, 7, 8]
    """
    queue = deque([start])
     
    while queue:
        vertex = queue.pop()
        yield vertex
        queue.extendleft(tree(vertex))

def dfsi(tree: Tree[T], start: T) -> Iterator[T]:
    """
    >>> tree = {1: (2, 3), 2: (4,), 3: (5, 6), 4: (7,), 5: (), 6: (8,), 7: (), 8: ()}
    >>> list(dfsi(partial(getitem, tree), 1))
    [1, 2, 4, 7, 3, 5, 6, 8]
    """
    stack = deque([iter([start])])
     
    while stack:
        try:
            vertex = next(stack[-1])
        except StopIteration:
            del stack[-1]
        else:
            yield vertex
            stack.append(iter(tree(vertex)))
 
def bfsi(tree: Tree[T], start: T) -> Iterator[T]:
    """
    >>> tree = {1: (2, 3), 2: (4,), 3: (5, 6), 4: (7,), 5: (), 6: (8,), 7: (), 8: ()}
    >>> list(bfsi(partial(getitem, tree), 1))
    [1, 2, 3, 4, 5, 6, 7, 8]
    """
    queue = deque([iter([start])])
     
    while queue:
        try:
            vertex = next(queue[-1])
        except StopIteration:
            del queue[-1]
        else:
            yield vertex
            queue.appendleft(iter(tree(vertex)))

def dfsr(tree: Tree[T], start: T) -> Iterator[T]:
    """
    >>> tree = {1: (2, 3), 2: (4,), 3: (5, 6), 4: (7,), 5: (), 6: (8,), 7: (), 8: ()}
    >>> list(dfsr(partial(getitem, tree), 1))
    [1, 2, 4, 7, 3, 5, 6, 8]
    """
    yield start
    
    for vertex in tree(start):
        yield from dfsr(tree, vertex)

def bfsr(tree: Tree[T], start: T) -> Iterator[T]:
    """
    >>> tree = {1: (2, 3), 2: (4,), 3: (5, 6), 4: (7,), 5: (), 6: (8,), 7: (), 8: ()}
    >>> list(it.islice(bfsr(partial(getitem, tree), 1), 8))
    [1, 2, 3, 4, 5, 6, 7, 8]
    """
    yield start
    
    for vertex in bfsr(tree, start):
        yield from tree(vertex)

def bfsr2(tree: Tree[T], start: T) -> Iterator[T]:
    """
    >>> tree = {1: (2, 3), 2: (4,), 3: (5, 6), 4: (7,), 5: (), 6: (8,), 7: (), 8: ()}
    >>> list(it.islice(bfsr2(partial(getitem, tree), 1), 8))
    call ID 0 begun
    call ID 0: yielding 1
    call ID 1 begun
    call ID 1: yielding 1
    call ID 0: yielding children of 1
    call ID 0: yielding 2
    call ID 0: yielding 3
    call ID 2 begun
    call ID 2: yielding 1
    call ID 1: yielding children of 1
    call ID 1: yielding 2
    call ID 0: yielding children of 2
    call ID 0: yielding 4
    call ID 1: yielding 3
    call ID 0: yielding children of 3
    call ID 0: yielding 5
    call ID 0: yielding 6
    call ID 3 begun
    call ID 3: yielding 1
    call ID 2: yielding children of 1
    call ID 2: yielding 2
    call ID 1: yielding children of 2
    call ID 1: yielding 4
    call ID 0: yielding children of 4
    call ID 0: yielding 7
    call ID 2: yielding 3
    call ID 1: yielding children of 3
    call ID 1: yielding 5
    call ID 0: yielding children of 5
    call ID 1: yielding 6
    call ID 0: yielding children of 6
    call ID 0: yielding 8
    [1, 2, 3, 4, 5, 6, 7, 8]
    """
    call_counter = it.count()

    def _bfs() -> Iterator[T]:
        call_id = next(call_counter)
        print(f'call ID {call_id} begun')
        print(f'call ID {call_id}: yielding {start}')
        yield start
        
        for vertex in _bfs():
            print(f'call ID {call_id}: yielding children of {vertex}')
        
            for child in tree(vertex):
                print(f'call ID {call_id}: yielding {child}')
                yield child
            
    yield from _bfs()

def bfsr3(tree: Tree[T], start: T) -> Iterator[T]:
    """
    >>> tree = {1: (2, 3), 2: (4,), 3: (5, 6), 4: (7,), 5: (), 6: (8,), 7: (), 8: ()}
    >>> list(bfsr3(partial(getitem, tree), 1))
    [1, 2, 3, 4, 5, 6, 7, 8]
    """
    yield start
    iterator = bfs(tree, start)
    level_size = 1
    
    while level_size:
        level = it.islice(iterator, level_size)
        level_size = 0
    
        for vertex in level:
            for child in tree(vertex):
                yield child
                level_size += 1

def depth_labelled(tree: Tree[T], start: T) -> tuple[Tree[tuple[T, int]], tuple[T, int]]:
    def new_tree(vertex_with_depth: tuple[T, int]) -> Iterator[tuple[T, int]]:
        vertex, depth = vertex_with_depth
        
        for child in tree(vertex):
            yield (child, depth + 1)

    return new_tree, (start, 0)

def depth_limited(tree: Tree[T], start: T, limit: int) -> tuple[Tree[tuple[T, int]], tuple[T, int]]:
    ntree, nstart = depth_labelled(tree, start)
    
    def new_tree(vertex_with_depth: tuple[T, int]) -> Iterator[tuple[T, int]]:
        vertex, depth = vertex_with_depth

        if depth < limit:
            yield from ntree(vertex_with_depth)

    return new_tree, nstart

def iddfs(tree: Tree[T], start: T) -> Iterator[T]:
    """
    >>> tree = {1: (2, 3), 2: (4,), 3: (5, 6), 4: (7,), 5: (), 6: (8,), 7: (), 8: ()}
    >>> list(iddfs(partial(getitem, tree), 1))
    [1, 2, 3, 4, 5, 6, 7, 8]
    """
    for limit in it.count():
        limited_tree = depth_limited(tree, start, limit)
        level_size = 0

        for vertex, depth in dfsr(*limited_tree):
            if depth == limit:
                yield vertex
                level_size += 1

        if not level_size:
            break

if __name__ == '__main__':
    import doctest
    doctest.testmod()
