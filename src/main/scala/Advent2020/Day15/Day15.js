ls = [0,8,15,2,12,1,4]
sz = 30000000

ages = new Array(sz).fill(0)

function gen(s=ls, end=2020) {
  let i;
  for (i=0;i<s.length;++i) ages[s[i]] = i
  let x = s.length
  ages[0] = x
  let y = ages[x]
  for (i=x+1; i < end-1; ++i) {
    ages[x] = i
    x = y !== 0 ? i - y : 0
    y = ages[x]
  }
  return x
}

function test() {
  t0 = performance.now()
  console.log(gen(ls,sz))
  console.log("Took: " + (performance.now() - t0)/1e3)
  ages = new Array(sz).fill(0)
}

test()