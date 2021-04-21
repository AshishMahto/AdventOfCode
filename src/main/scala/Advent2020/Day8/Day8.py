
program = [(op, int(i))
           for ln in open("input.txt").readlines()
           for op, i in (ln.split(),)]

def run(prog = tuple(program[:]), edits=None):
  if edits: prog[edits[0]] = edits[1]
  acc = 0
  instr = 0
  visited = set()
  while instr not in visited:
    visited.add(instr)
    if instr == len(prog): return acc, True
    if instr >  len(prog): return acc, False
    op, i = prog[instr]
    if op == "acc": acc += i
    if op == "jmp": instr += i - 1
    instr += 1
  return acc, False

print(run())

print(
  [
    ret
    for i, (op, v) in enumerate(program)
    if op == "jmp"
    for prog2 in [program[:]]
    for ret, is_good in [run(prog2, (i, ["nop", 0]))]
    if is_good
  ]
)

