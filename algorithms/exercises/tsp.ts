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
export function numSetBits(n: number): number {
  let setBits = 0
  let x = n
  while(x > 0) {
    x &= (x - 1)
    setBits++;
  }

  return setBits
}

export function setBits(n: number): number[] {
  const res = []

  let x = n
  while(x != 0) {
    const bitNum = Math.floor(Math.log2(x))
    res.push(bitNum)
    x-=(2**bitNum)
  }

  return res
}

export function factorial(n: number): number {
  let x = n
  for(let i=n-1; i>0; i--) {
    x *= i
  }
  return x
}

export function subsets<a>(xs: a[], subsetSize: number): a[][] {
  if(subsetSize === 1) {
    return xs.map((x) => { return [x] })
  } else if (subsetSize == xs.length) {
    return [xs]
  }

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
  const start: Point = points[0]
  const paths: Map<[Point[], Point], number> = new Map()

  for(let m = 2; m < points.length; m++) {
    const subs = subsets(points, m).filter((xs) => { return xs.includes(start) } )

    subs.forEach((xs) => {
      for(const j of xs) {
        if( j === start ) { continue }

        // loop over all of the elements of xs, except for j. Find the minimum distance to j for this subset
        const withoutJ = xs.filter((x) => { return x != j } )
        const distances: number[] = withoutJ.map( (k) => { return  (paths.get([withoutJ, j]) || Infinity) + euclideanDistance(k, j)})
        paths.set([xs, j], Math.min(...distances))
      }
    })
  }

  // extract minimum tour from the points
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
