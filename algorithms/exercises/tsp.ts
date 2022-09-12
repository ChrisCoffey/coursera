import * as fs from 'fs'
import internal from 'stream'

type Point = {x: number, y: number}

function lexComparePoints(a: Point, b: Point): number {
  if(a.x < b.x) {
    return -1
  } else if (a.x > b.x) {
    return 1
  } else if (a.x == b.x && a.y < b.y) {
    return -1
  } else if (a.x == b.x && a.y > b.y){
    return 1
  } else {
    return 0
  }
}

function angleComparePoints(p: Point) : (a: Point, b: Point) => number {
  return (a: Point, b: Point) => {
    const angleA = findAngle(p, a)
    const angleB = findAngle(p, b)

    if(angleA < angleB) {
      return -1
    } else if (angleA > angleB) {
      return 1
    } else {
      return 0
    }
  }
}

function encodePoints (points: Point[]): string {
  let str = ""
  points.sort(lexComparePoints).forEach((p) => {
    str += `(${p.x}, ${p.y}),`
  })

  return str
}

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

export function subsets<a>(xs: a[], subsetSize: number, pred?: (x: number) => boolean ): a[][] {
  if(subsetSize === 1) {
    return xs.map((x) => { return [x] })
  } else if (subsetSize == xs.length) {
    return [xs]
  }
  const f = pred || ((x: number) => { return true })

  const result: a[][] = []

  for(let x=0; x < (2 ** (xs.length)) -1; x++) {
    if(numSetBits(x) == subsetSize) {
      let valid = false
      const subSet = setBits(x).map( (i) => {
        valid = f(i)
        return xs[i]
      })
      if(valid) {
        result.push(subSet)
      }
    }
  }

  return result
}

interface Paths {
  [index: string]: number
}

function dynamicTSPLength(points: Point[]): number {
  const start: Point = points[0]
  const paths: Paths = {}
  const pred =  (x: number) => { return x === 0 }

  for(let m = 2; m < points.length; m++) {
    const subs = subsets(points, m, pred)
    console.log(`Exploring paths of length ${m}. There are ${subs.length} subsets.`)

    subs.forEach((xs) => {
      for(const j of xs) {
        if( j === start ) { continue }

        // loop over all of the elements of xs, except for j. Find the minimum distance to j for this subset
        const withoutJ = xs.filter((x) => { return x != j } )
        const distances: number[] = withoutJ.map( (k) => {
          return  (paths[encodePoints(withoutJ) + k.toString()] || Infinity) + euclideanDistance(k, j)
        })

        paths[encodePoints(xs) + j.toString()] = Math.min(...distances)
      }
    })
  }

  // Find the minimum length tour
  const finalHops = points.slice(1).map((point) => {
    const pathLength = paths[encodePoints(points) + point.toString()] || Infinity
    return pathLength + euclideanDistance(point, start)
  })

  return Math.min(...finalHops)
}

type ConvexHull = Point[]

export function grahamScan(points: Point[]): ConvexHull {
  let p: Point = {x: Infinity, y: Infinity}
  points.forEach((point) => {
    if(point.y < p.y) {
      p = point
    } else if (point.y == p.y && point.x > p.x) {
      p = point
    }
  })

  const sortedByAngle: Point[] = points.sort(angleComparePoints(p))

  const hull: Point[] = []
  for(let i = 0; i < sortedByAngle.length; i++) {
    if(i <= 1) {
      hull.push(sortedByAngle[i])
      continue
    }

    // cross product is < 0 during a right-hand turn
    while(hull.length > 1 && crossProduct(hull[hull.length -2], hull[hull.length - 1], sortedByAngle[i]) < 0) {
      // During a right-hand turn, the last element in the stack needs to be discarded because it lies inside
      // the convex hull
      hull.pop()
    }
    hull.push(sortedByAngle[i])
  }

  return hull
}

export function crossProduct(a: Point, b: Point, c: Point): number {
  return ((b.x - a.x) * (c.y - a.y)) - ((b.y - a.y) * (c.x - a.x))
}

// little bit of trig here
export function findAngle(a: Point, b: Point): number {
  const opposite = b.y - a.y
  const adjacent = b.x - a.x
  const hypotenuse = Math.sqrt(opposite**2 + adjacent**2)
  return Math.asin(opposite / hypotenuse)
}

type InsertionPoint = {i: Point, j: Point, r: Point}
function convexHullTSPLength(points: Point[]): Point[] {
  const path = grahamScan(points)
  const findInternalPoints = () => {
    return points.filter((point) => { return !path.includes(point) })
  }

  const pathDelta = (i: Point, next: Point, r: Point) => {
    const currentLength = euclideanDistance(i, next)
    const costIR = euclideanDistance(i, r)
    const costRNext = euclideanDistance(r, next)
    return (costIR + costRNext) - currentLength
  }

  while(path.length != points.length) {
    console.log("step", path.length)
    const internalPoints: Point[] = findInternalPoints()

    // Calculate the distance added to the path length by inserting pointR inbetwen
    // i and i+1 in the path.
    const minimalInsertions: InsertionPoint[] = internalPoints.map( (pointR) => {
      let minStart: number = 0
      let minCost: number = Infinity
      for(let i = 0; i< path.length; i++) {
        const next: number = i == path.length - 1 ? 0 : i + 1
        // The actual path length delta calculations
        const costIRNext = pathDelta(path[i], path[next], pointR)

        // Keep track of the shortest "extra length" across all insertion points
        if(costIRNext < minCost) {
          minCost = costIRNext
          minStart = i
        }
      }

      return {i: path[minStart], r: pointR, j: path[minStart == path.length -1 ? 0 : (minStart + 1)]}
    })

    let minExtraLength = Infinity
    let r: InsertionPoint = {i: path[0], j: path[0], r: path[0]} // TODO figure out how to improve compiler-speak
    minimalInsertions.forEach((insertion) => {
      console.log(insertion)
      const deltaRatio =
        (euclideanDistance(insertion.i, insertion.r) + euclideanDistance(insertion.r, insertion.j)) /
        euclideanDistance(insertion.i, insertion.j)

      if(deltaRatio < minExtraLength) {
        r = insertion
      }
    })

    const indexI = path.findIndex((x) => { return x === r.i} )
    let shifted = path[indexI + 1]
    path[indexI + 1] = r.r
    for(let n = indexI + 2; n < path.length; n++) {
      const x = shifted
      shifted = path[n]
      path[n] = x
    }
  }

  return path
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

const points = loadMap("data/tsp.txt")
console.log(points)

console.log(grahamScan(points))
console.log(convexHullTSPLength(points))


