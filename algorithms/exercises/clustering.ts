import * as fs from 'fs'
import { UnionFind } from './union_find'

type Edge = {a: number, b: number, cost: number}

function maxSpacingKClustering(dataPath: string, k: number): number {
  const edges = processEdgeFile(dataPath)
  const distinctPoints = new Set<number>()
  edges.forEach(e => { distinctPoints.add(e.a); distinctPoints.add(e.b) })

  const unionFind = new UnionFind<number>(Array.from(distinctPoints.values()))
  let numClusters: number = distinctPoints.size
  const sortedEdges: Edge[] = edges.sort((a,b) => {return a.cost - b.cost})

  // console.log(sortedEdges)

  while(numClusters > k) {
    if(sortedEdges.length == 0) {
      break
    }
    const edge: Edge | undefined = sortedEdges.shift()
    if(edge === undefined) {
      break
    }

    if(unionFind.union(edge.a, edge.b)) {
      numClusters--
    } else {
    }
  }

  const spacing = sortedEdges.find(edge => unionFind.union(edge.a, edge.b))


  unionFind.prettyPrint()
  return spacing?.cost || -1
}

function processEdgeFile(path: string): Edge[] {
  const rawLines = fs.readFileSync(path).toString().split("\n").filter(x => x.length > 0)
  const edges = rawLines.map(l => {
    const [a,b,cost, ..._]: string[] = l.split(" ")
    return {a: parseInt(a), b: parseInt(b), cost: parseInt(cost)}
  })
  return edges
}

console.log(maxSpacingKClustering("data/clustering1.txt", 4))


// process the 200k 24bit permutations. The distance between points is given by the Hamming distance between each pair. There are 40B pairs (200k ^ 2), which
// won't fit in my machine's memory.


function computeBinaryClusteringMaxK(path: string): number {
  // Preprocessing
  const hextets = processBinaryNodes(path)
  let index = {
    a: new Map<number, Set<number>>(),
    b: new Map<number, Set<number>>(),
    c: new Map<number, Set<number>>(),
    d: new Map<number, Set<number>>()
    }

  hextets.forEach((h: Hextet) => {
    index = indexHextet(index, h)
  })

  // Compute the edges
  let edges : Edge[] = []
  hextets.forEach(
    (h) => {
      edges = edges.concat(computeEdgesFromIndex(index, h))
  })
  const sortedEdges = edges.sort(
    (a, b) => {return a.cost - b.cost}
  )

  // Run the clustering algorithm, tracking how many clusters remain after processing all valid edges
  const unionFind = new UnionFind<number>(Array.from(hextets.map((h) => {return h.value})))
  let numClusters = hextets.length
  sortedEdges.forEach((edge) => {
    if(unionFind.union(edge.a, edge.b)) { numClusters-- }
  })

  return numClusters
}

function computeBinaryEdges(nodesPresent: boolean[]): Edge[] {
  // Set a bit in an array for each num, or use a Set<number>
  for(let i=0; i < nodesPresent.length; i++) {
    //  compute all one and two bit permutations, and check if they're in the array
    //  if there's a match, add an edge

  }

  return []
}

function checkPermutations(nodesPresent: boolean[], x: number): Edge[] {


  return []
}

function binaryNodeSet(path: string): boolean[] {
  const rawLines = fs.readFileSync(path).toString().split("\n")
  const result: boolean[] = []

  rawLines.forEach( l => {
    const littleEndianBits: number[] = l.split(" ").map(bit => Number.parseInt(bit))
    const n: number = bitsToNumber(littleEndianBits)
    result[n] = true
  })

  return result
}


type Hextet = {value: number, a: number, b: number, c: number, d: number}

function processBinaryNodes(path: string): Hextet[] {
  const rawLines = fs.readFileSync(path).toString().split("\n")

  const nodes = rawLines.map( l => {
    const littleEndianBits: number[] = l.split(" ").map(bit => Number.parseInt(bit))
    return {
      value: bitsToNumber(littleEndianBits.reverse()),
      a: bitsToNumber(littleEndianBits.slice(0, 6)),
      b: bitsToNumber(littleEndianBits.slice(6, 12)),
      c: bitsToNumber(littleEndianBits.slice(12, 18)),
      d: bitsToNumber(littleEndianBits.slice(18, 24))
    }
  })
  return nodes
}

type IndexSlice = Map<number, Set<number>>
type HextetIndex = {a: IndexSlice, b: IndexSlice, c: IndexSlice, d: IndexSlice}
function indexHextet(index: HextetIndex, h: Hextet): HextetIndex {
  let seen: Set<number> = index.a.get(h.a) || new Set<number>()
  seen.add(h.value)
  index.a.set(h.a, seen)

  seen = index.b.get(h.b) || new Set<number>()
  seen.add(h.value)
  index.b.set(h.b, seen)

  seen = index.c.get(h.c) || new Set<number>()
  seen.add(h.value)
  index.c.set(h.c, seen)

  seen = index.d.get(h.d) || new Set<number>()
  seen.add(h.value)
  index.d.set(h.d, seen)

  return index
}

// This could be more efficient
function hextetPairs (index: HextetIndex, h: Hextet): [IndexSlice, IndexSlice, number, number][] {
  return [
    [index.a, index.b, h.a, h.b],
    [index.a, index.c, h.a, h.c],
    [index.a, index.d, h.a, h.d],
    [index.b, index.c, h.b, h.c],
    [index.b, index.d, h.b, h.d],
    [index.c, index.d, h.c, h.d]
  ]
}

function computeEdgesFromIndex(index: HextetIndex, h: Hextet): Edge[] {
  const candidates: Set<number> = new Set<number>()

  hextetPairs(index, h).forEach(
    ([left, right, hl, hr]) => {
      const ls: Set<number> = left.get(hl) || new Set<number>()
      const rs: Set<number> = right.get(hr)|| new Set<number>()
      // This is set intersection.
      const xs: number[] = Array.from([...ls].filter(x => rs.has(x)))
      xs.forEach(
        (v) => {
          candidates.add(v)
      })
  })

  return Array.from(candidates.values())
    .filter(
      (node: number) => {return isCloseEnough(h, node) })
    .map(
      (node: number) => { return {a: h.value, b: node, cost: hammingWeight(h.value ^ node)} })
}

function isCloseEnough(h: Hextet, node: number): boolean {
  const dist = hammingWeight(h.value ^ node)
  return (dist <= 2)
}


// This is an awesome algorithm. It takes advantage of what subtracting 1 does to binary numbers!
// Namely, it flips all the bits to the right of the rightmost set bit.
function hammingWeight(n: number): number {
  let count:number = 0
  while(n > 0) {
    n &= (n - 1)
    count++
  }
  return count
}

function bitsToNumber(bits: number[]): number {
  if(bits.length === 0) { return -1 }
  return bits.reduce(
    (acc, x, i) => acc + ((2*x) ** i))
}


console.log(computeBinaryClusteringMaxK("data/clustering_big.txt"))
