import * as fs from 'fs'

type Point = {x: number, y: number}

function euclideanDistance(a: Point, b: Point): number {
  return Math.sqrt((a.x - b.x)**2 + (a.y - b.y)**2)
}

function normalizePoints(points: Point[]): Point[] {
  const minX = Math.min(...points.map((p) => { return p.x}))
  const minY = Math.min(...points.map((p) => { return p.y}))

  return points.map((p) => {
    return {x: p.x - minX, y: p.y - minY}
  })
}

// This is Kernighan's algorithm
function numSetBits(n: number): number {
  let setBits = 0
  let x = n
  while(x > 0) {
    x &= (x - 1)
    setBits++;
  }

  return setBits
}

function setBits(n: number): number[] {
  const res = []

  let x = n
  while(x != 0) {
    const bitNum = Math.floor(Math.log2(x))
    res.push(bitNum)
    x-=(2**bitNum)
  }

  return res
}

function subsets<a>(xs: a[], subsetSize: number): a[][] {
  const result: a[][] = []

  for(let x=0; x < (2 ** (xs.length)) -1; x++) {
    if(numSetBits(x) == subsetSize) {
      const subSet = setBits(x).map( (i) => {return xs[i] })
      result.push(subSet)
    }
  }

  return result
}

function dynamicTSPLength(points: Point[]): number {

  return 42
}


function loadMap(path: string): Point[] {
  const rawLines = fs.readFileSync(path).toString().split("\n")
  const count = rawLines.shift()
  rawLines.pop() // remove the trailing newline

  const points = rawLines.map((line) => {
    const nums = line.split(" ")
    return {
      x: Number.parseFloat(nums[0]),
      y: Number.parseFloat(nums[1])
    }
  })

  return points
}

function prettyPrintPoints(points: Point[]): string {
  let str = ""
  points.forEach((p) => {
    str += `(${p.x}, ${p.y}),`
  })

  return str
}

console.log(loadMap("data/tsp.txt"))


console.log(numSetBits(3), setBits(3))
console.log(subsets([1,2,3,4], 4))
