from collections import Counter
from math import prod

def parse(tile):
  k,v = tile.split(':\n')
  return int(k.split(' ')[1]), tuple(v.split('\n'))

inp = dict(map(parse, open("input.txt").read().strip().split('\n\n')))

def enc(border):
  s: str = border.replace('#', '1').replace('.', '0')
  return int(s,2)

join = ''.join
def mapL(f, l): return list(map(f, l))
def revs(s: str): return join(reversed(s))

def borders(tile):
  ls = [tile[0], tile[-1]]
  tile = list(zip(*tile))
  ls.extend(map(join, [tile[0], tile[-1]]))
  ls.extend(mapL(revs, ls))
  return mapL(enc, ls)

def orientations(tile):
  tile = list(tile)
  for _ in range(2):
    for _ in range(2):
      for _ in range(2):
        yield tile
        tile = list(reversed(tile))
      tile = mapL(revs, tile)
    tile = mapL(join, zip(*tile))
orients = { k: tuple(orientations(tile)) for k,tile in inp.items() }

freqs = Counter(sum(map(borders, inp.values()),start=[]))

ones = { k for k, v in freqs.items() if v != 2 }

uneven = { k:v for k,tile in inp.items() for v in [ones & set(borders(tile))] if v }

corners = [k for k, v in uneven.items() if len(v) == 4]

print(corners, prod(corners))

sz = 12
brd = [[None] * sz for _ in range(sz)]

def fit(k, y=None, x=None):
  def bot(tile): return enc(tile[-1])
  def aft(tile): return enc(join(ln[-1] for ln in tile))
  abv = {bot(brd[y-1][x])} if y else uneven.get(k)
  bef = {aft(brd[y][x-1])} if x else uneven.get(k)
  if not (abv and bef): return
  for pc in orients[k]:
    top = enc(pc[0])
    lft = enc(join(ln[0] for ln in pc))
    if top in abv and lft in bef:
      return pc

unvisited = set(inp.keys())

for y in range(sz):
  for x in range(sz):
    ks, fits = zip(*[(k, v) for k in unvisited for v in [fit(k, y, x)] if v])
    if (y,x) != (0,0) and (y,x) != (0,1):
      assert len(fits) == 1, ks
    brd[y][x] = fits[0]
    unvisited.remove(ks[0])

brd = [[[join(ln[1:-1]) for ln in tile[1:-1]] for tile in row] for row in brd]

def fmt(brd): return [
  join(tile[y2] for tile in brd[y1])
  for y1 in range(sz)
  for y2 in range(len(brd[0][0][0]))
]

sea_monster = [
  "                  # ",
  "#    ##    ##    ###",
  " #  #  #  #  #  #   "
]

def toSet(brd,x0=0,y0=0): return {
  (x+x0,y+y0)
  for y,ln in enumerate(brd)
  for x,c in enumerate(ln)
  if c == '#'
}

def go(brd):
  brdSet = toSet(brd)
  found = 0
  for x in range(len(brd)):
    for y in range(len(brd)):
      seaMon = toSet(sea_monster,x,y)
      if seaMon <= brdSet:
        brdSet -= seaMon
        found += 1
  return len(brdSet), found

print(min(map(go, orientations(fmt(brd)))))
