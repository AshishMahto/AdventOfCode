import functools

import numpy

init = (0, 128), (0, 8)

def step(prev, cmd):
  (y1, y2), (x1, x2) = prev
  ym = (y2 + y1) // 2
  xm = (x2 + x1) // 2
  if cmd == 'F': y2 = ym
  if cmd == 'B': y1 = ym
  if cmd == 'L': x2 = xm
  if cmd == 'R': x1 = xm
  return (y1, y2), (x1, x2)

f = open("input.txt").read().strip().split()

ls = set()
for ln in f:
  (y, _), (x, _) = functools.reduce(step, ln, init)
  ls.add(y * 8 + x)

print(max(ls))

seats = { y * 8 + x for y in range(128) for x in range(8) }

print(min(seats - ls, key=lambda a: abs(512 - a)))
