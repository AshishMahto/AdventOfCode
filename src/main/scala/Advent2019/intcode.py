from collections import defaultdict


class IntCode:
  instrs = ()
  def __init__(self, instrs=None):
    self.instrs = tuple(instrs or map(int, open("input.txt").read().split(',')))

  def run(self, *inits):
    mem = defaultdict(int)
    for k,v in enumerate(self.instrs): mem[k] = v
    for k,v in inits: mem[k] = v
    def rec():
      i = 0
      while True:
        cmd, x, y, z = map(lambda j: mem[j], range(i, i+4))
        if cmd == 99: return
        elif cmd ==  1: mem[z] = mem[x] + mem[y]
        elif cmd ==  2: mem[z] = mem[x] * mem[y]
        else: raise ValueError
        i += 4
    def asList(): return [mem[i] for i in range(max(mem.keys()) + 1)]
    rec()
    return asList()