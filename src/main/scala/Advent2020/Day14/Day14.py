
inp = open("input.txt").read()

def parseMask(l, r: "str"):
  assert l == "mask"
  return r, int(r.replace('X', '0'), 2), int(r.replace('X', '1'), 2)

def allMasks(addr: int, mask: str):
  rets = []
  mask = ''.join(reversed(mask))
  def rec(i=0, acc=0):
    if i == len(mask): return rets.append(acc)
    if mask[i] == '0': rec(i+1, acc | (addr & (1 << i)))
    if mask[i] in '1X': rec(i+1, acc | (1 << i))
    if mask[i] == 'X': rec(i+1, acc)
  rec()
  # print(''.join(reversed(f"{addr:0b}")), mask, "\n", rets)
  return rets


def parseMem(l: "str", r: "str"):
  b = l.index(']')
  return int(l[4:b]), int(r)

def go(lns=inp):
  mask = None
  mem = {}
  for ln in lns.strip().split('\n'):
    s = ln.split(' = ')
    try:
      _, *m = parseMask(*s)
      mask = m
    except:
      addr, val = parseMem(*s)
      mem[addr] = (val | mask[0]) & mask[1]
  return mem


print(sum(go().values()))


def part2(lns=inp):
  mask = None
  mem = {}
  for ln in lns.strip().split('\n'):
    s = ln.split(' = ')
    try:
      m, *_ = parseMask(*s)
      mask = m
    except:
      addr, val = parseMem(*s)
      for k in allMasks(addr, mask):
        mem[k] = val
  return mem

print(part2("""mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1
"""))

print(sum(part2().values()))
