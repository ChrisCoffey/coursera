import * as h from './heap'
import * as fs from 'fs'

function computeMWIS(path: string): number[] {
  const values = parseMWIS(path)

  const mwisToN : number[] = []
  // 1 2 3
  for(let i =0; i < values.length; i++) {
    if(i === 0) {
      mwisToN[i] = values[i]
    }
    else if(i === 1) {
      (mwisToN[i-1] > values[i]) ? (mwisToN[i] = mwisToN[i-1]) : (mwisToN[i] = values[i])
    }
    else if(mwisToN[i-2] + values[i] > mwisToN[i-1]){
      mwisToN[i] = (mwisToN[i-2] + values[i])
    }
    else {
      mwisToN[i] = mwisToN[i-1]
    }
  }

  return mwisToN
}


function parseMWIS(path: string): number[] {
  const rawText = fs.readFileSync(path).toString()
  const lines = rawText.split("\n")
  lines.pop()
  lines.shift()

  return lines.map((x) => {return parseInt(x)})
}

function take<A>(n: number, xs: A[]): A[] {
  const res = []
  for(let i=0; i < n && i < xs.length; i++) { res[i] = xs[i]}

  return res
}

function check(indices: number[], mwis: number[]): boolean[] {
  const res : boolean[] = []
  let n = res.length - 1
  for(let i=mwis.length -1; i > 0; i--) {
    if(!indices.includes(i)) { continue }

    res[n] = (mwis[i-1] != mwis[i])
    n--
  }

  return res
}

const mwis = computeMWIS("data/mwis.txt")
const indicesToCheck = [1,2,3,4,17,117,517,997]

take(10, mwis).forEach((x) => console.log(x))

console.log(check(indicesToCheck, mwis))
