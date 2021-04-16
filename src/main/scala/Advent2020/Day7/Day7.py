
import requests
import builtins

def dl(i, cookie="53616c7465645f5f20e9261536f734605b8f8b979cea01779a9cd3dc55811cecf2604a9358df1bda489dfede22dc1461"):
  r = requests.get(f'https://adventofcode.com/2020/day/{i}/input', cookies=dict(session=cookie))
  with builtins.open(f"Day{i}/input.txt", 'wb') as f:
    f.write(r.content)
