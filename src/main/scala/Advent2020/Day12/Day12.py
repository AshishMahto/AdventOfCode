
dirs = "ESWN"

vel = {
  'E': ( 0, 1),
  'W': ( 0,-1),
  'N': ( 1, 0),
  'S': (-1, 0)
}

def add(p, q): return tuple(i + j for i,j in zip(p, q))
def scale(k, p): return tuple(k * i for i in p)

def parse(s):
  dir, *num = s
  return dir, int(''.join(num))

inp = map(parse, open("input.txt").read().strip().split())

def go():
  cur = 0
  pos = (0, 0)
  for d, i in inp:
    if d in vel:
      pos = add(pos, scale(i, vel[d]))
    elif d == 'F':
      pos = add(pos, scale(i, vel[dirs[cur]]))
    else:
      i //= 90
      if d == 'L':
        i = 4 - i
      cur = (cur + i) % 4
  return pos

part1 = go()
print(f"{part1}: {sum(map(abs, part1))}")

import numpy as np

def part2():
  def vec(*tpl): return np.array([[_] for _ in tpl])
  mv = { k: vec(*v) for k, v in vel.items() }
  pos = vec(0, 0)
  wp = vec(0, 0)
  for d, i in inp:
    if d in mv:
      wp += i * mv[d]
    elif d == 'F':
      pos += i * np.linalg.norm(wp)
    else:
      i //= 90
      if d == 'L':
        i = 4 - i
      cur = (cur + i) % 4
  return pos

  pass
