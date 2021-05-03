import re

rules, inps = open("input.txt").read().strip().split('\n\n')
inps = inps.split()

def parseRule(s):
  k,vs = s.split(': ')
  k = int(k)
  if '"' in vs: return k, vs.replace('"', '')
  vs = vs.split(' | ')
  vs = [[int(x) for x in v.split(' ')] for v in vs]
  return k,vs

rules = dict(map(parseRule, rules.split('\n')))

compiled = {}

def pile(k):
  rule = rules[k]
  if k in compiled: return compiled[k]
  elif isinstance(rule, str): compiled[k] = rule
  elif len(rule) == 1:
    compiled[k] = ''.join(pile(x) for x in rule[0])
  else:
    compiled[k] = '(?:' + '|'.join(
      ''.join(pile(x) for x in ln)
      for ln in rule
    ) + ')'
  # print(f"compiled[{k: 4}] = '{compiled[k]}'")
  return compiled[k]

compiled[113] = '.'
compiled[112] = '.a'
compiled[130] = 'a.'
compiled[  6] = 'b.'
compiled[132] = '.b'

pile(0)
r4 = compiled[42]
r3 = compiled[31]

# print(len(compiled[0]))

r = re.compile('^' + compiled[0] + '$')
print(sum(r.match(ln) is not None for ln in inps))


rs = [
  re.compile('^' + r4 + '+' + r4 + s + r3 + s + '$')
  for i in range(1, 7)
  for s in ['{' + str(i) + '}']
]
def check(ln): return any(r.match(ln) is not None for r in rs)

print(sum(map(check, inps)))
