
inp = open("input.txt").read()

def parseMask(l, r):
  assert l == "mask"
  return r, int(r.replace('X', '0'), 2), int(r.replace('X', '1'), 2)

def allMasks(addr, mask):
  rets = []
  mask = ''.join(reversed(mask))
  def rec(i=0, acc=0):
    if i == len(mask): return rets.append(acc)
    if mask[i] == '0': rec(i+1, acc | (addr & (1 << i)))
    if mask[i] in '1X': rec(i+1, acc | (1 << i))
    if mask[i] == 'X': rec(i+1, acc)
  rec()
  return rets


def parseMem(l, r):
  b = l.index(']')
  return int(l[4:b]), int(r)

def part1(lns=inp):
  mask = None
  mem = {}
  for ln in lns.strip().split('\n'):
    s = ln.split(' = ')
    try:
      mask = parseMask(*s)[1:]
    except:
      addr, val = parseMem(*s)
      mem[addr] = (val | mask[0]) & mask[1]
  return mem


print(sum(part1().values()))


def part2(lns=inp):
  mask = None
  mem = {}
  for ln in lns.strip().split('\n'):
    s = ln.split(' = ')
    try:
      mask = parseMask(*s)[0]
    except:
      addr, val = parseMem(*s)
      for k in allMasks(addr, mask): mem[k] = val
  return mem

print(sum(part2().values()))
