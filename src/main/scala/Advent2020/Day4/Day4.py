import re


def toInt(s, x):
  def helper():
    if s == "pid": return x
    try:
      if x.endswith("cm"): return int(x[:-2]), 'cm'
      if x.endswith("in"): return int(x[:-2]), 'in'
      return int(x)
    except: return x
  return s, helper()

f = [
  dict(sorted(map(lambda s: toInt(*s.split(':')), s.split())))
  for s in open("input.txt").read().strip().split('\n\n')
]

reqs = set("byr,iyr,eyr,hgt,hcl,ecl,pid".split(','))
print(sum(set(ln.keys()) >= reqs for ln in f))

hcl = re.compile(r"#[0-9a-f]{6}")
ecl = re.compile(r"amb|blu|brn|gry|grn|hzl|oth")
pid = re.compile(r"[0-9]{9}")

def validate(d):
  try:
    assert 1920 <= d["byr"] <= 2002
    assert 2010 <= d["iyr"] <= 2020
    assert 2020 <= d["eyr"] <= 2030
    ht, unit = d["hgt"]
    if unit == 'cm': assert 150 <= ht <= 193
    if unit == 'in': assert 59 <= ht <= 76
    assert hcl.fullmatch(d["hcl"])
    assert ecl.fullmatch(d["ecl"])
    assert pid.fullmatch(d["pid"])
    return 1
  except:
    return 0

print(sum(map(validate, f)))
