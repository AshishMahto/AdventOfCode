
import itertools as it
from collections import Counter

def p(x):
  print(x)
  return x

state = frozenset(
  (x, y, 0)
  for y, ln in enumerate((open("input.txt").read().strip() or '.#.\n..#\n###').split())
  for x, cell in enumerate(ln)
  if cell == '#'
)

dim = 3

def plus(*lss): return tuple(map(sum, zip(*lss)))
def scale(k, ls): return tuple(k * x for x in ls)

def basis(i,k=1):
  ls = [0] * dim
  ls[i] = k
  return tuple(ls)

def nbrs(p): return tuple(
  plus(p, q)
  for q in it.product((-1, 0, 1), repeat=dim)
  if q != ((0,) * dim)
)

def step(state=state):
  all_nbrs = tuple(map(nbrs, state))
  freqs = Counter(sum(all_nbrs,start=()))
  def debug(pt, f):
    if pt == (2,1,0): print(pt, f)
  return frozenset(
    pt
    for pt, f in freqs.items()
    # for _ in [debug(pt, f)]
    if f == 3 or (pt in state and f == 2)
  )

def printState(st):
  def getRange(i):
    ls = [p[i] for p in st]
    return range(min(ls), max(ls)+1)
  ys = getRange(1)
  xs = getRange(0)
  lns = []
  for z in getRange(2):
    lns.append(f'z={z}')
    for y in ys:
      s = ''.join('#' if (x,y,z) in st else '.' for x in xs)
      lns.append(f"|{s}|")
    lns.append('')
  lns.append('\n')
  print('\n'.join(lns))

def part1(state=state):
  for _ in range(6):
    # print(f'step{_}\n')
    # printState(state)
    state = step(state)
  return state

print(len(part1()))
