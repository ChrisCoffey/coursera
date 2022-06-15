import * as h from './heap'
import * as fs from 'fs'

function computeMWIS(values: number[]): number[] {
  const mwisToN : number[] = []
  // 1 2 3
  for(let i = 0; i < values.length; i++) {
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

function check(indices: number[], mwis: number[], values: number[]): boolean[] {
  const reconstructed: boolean[] = []

  let i = mwis.length - 1
  while(i >= 0) {
    // This edge cased tripped me up w/ wrapping around the array to the start again...
    if(i==0) {
      reconstructed[i] = true
      i--
      continue
    }

    if(mwis[i] == mwis[i-1]) {
      i--
    } else {
      reconstructed[i] = true
      i-=2
    }

  }

  const res : boolean[] = []
  indices.forEach((i,idx) => {
    res[idx] = reconstructed[i-1]
    console.log([i, idx, reconstructed[i-1]])
  })


  return res
}

const values = parseMWIS("data/mwis.txt")
const mwis = computeMWIS(values)
const indicesToCheck = [1,2,3,4,17,117,517,997]

take(10, mwis).forEach((x) => console.log(x))

console.log(check(indicesToCheck, mwis, values))
