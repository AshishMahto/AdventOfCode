
import itertools as it
import operator as op

brd = {
  (y, x): c
  for y, ln in enumerate(open("input.txt").read().strip().split())
  for x, c  in enumerate(ln)
}
w = max([x for _,x in brd.keys()]) + 1
h = max([y for y,_ in brd.keys()]) + 1

def map_brd(b):
  def occupied_nbrs(y, x):
    def first(i,j):
      k = 1
      while b.get((y+k*i, x+k*j)) == '.': k += 1
      return b.get((y+k*i, x+k*j)) == '#'
    return sum(
      (i,j) != (0,0) and first(i, j)
      for i in range(-1, +2)
      for j in range(-1, +2)
    )
  def step(pt, c):
    num = occupied_nbrs(*pt)
    if c == '.': return '.'
    if num == 0: return '#'
    if num >= 5: return 'L'
    return c
  return { pt: step(pt, c) for (pt, c) in b.items() }


def pretty(b):
  return '\n'.join(
    ''.join(
      b[y, x]
      for x in range(w)
    )
    for y in range(h)
  )

prev = None
cur = brd
while prev != cur:
  prev = cur
  cur = map_brd(cur)

print(sum(x == '#' for x in cur.values()))
