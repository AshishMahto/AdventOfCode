
import itertools as it
from collections import Counter

def go(dim):
  def init(): return frozenset(
    (x, y) + ((0,) * (dim - 2))
    for y, ln in enumerate((open("input.txt").read().strip()).split())
    for x, cell in enumerate(ln)
    if cell == '#'
  )

  def plus(*pts): return tuple(map(sum, zip(*pts)))

  def nbrs(p): return tuple(
    plus(p, q)
    for q in it.product((-1, 0, 1), repeat=dim)
    if q != ((0,) * dim)
  )

  def step(state):
    all_nbrs = tuple(map(nbrs, state))
    freqs = Counter(sum(all_nbrs,start=()))
    return frozenset(
      pt
      for pt, f in freqs.items()
      if f == 3 or (pt in state and f == 2)
    )

  state=init()
  for _ in range(6): state = step(state)
  return state

print(len(go(3)))

print(len(go(4)))
