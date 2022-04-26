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

  //console.log(sortedEdges)

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
  const edges = hextets.flatMap((h) => {return computeEdgesFromIndex(index, h) })
  const sortedEdges = edges.sort((a, b) => {return a.cost - b.cost})

  // Run the clustering algorithm, tracking how many clusters remain after processing all valid edges
  const unionFind = new UnionFind<number>(Array.from(hextets.map((h) => {return h.value})))
  let numClusters = hextets.length
  sortedEdges.forEach((edge) => {
    if(unionFind.union(edge.a, edge.b)) { numClusters-- }
  })

  return numClusters
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
  function updateIndex(key: string) {
    const seen: Set<number> = index[key].get(h[key]) || new Set<number>()
    seen.add(h.value)
    index[key].set(h[key], seen)
  }

  ["a", "b", "c", "d"].forEach(updateIndex)

  return index
}

// This could be more efficient
const hextetPairs = [["a", "b"], ["a", "c"], ["a", "d"], ["b", "c"], ["b", "d"], ["c", "d"]]
function computeEdgesFromIndex(index: HextetIndex, h: Hextet): Edge[] {
  const candidates: Set<number> = new Set<number>()

  hextetPairs.forEach(([left, right]) => {
    const ls: Set<number> = index[left].get(h[left])
    const rs: Set<number> = index[right].get(h[right])
    // This is set intersection.
    const xs: number[] = Array.from([...ls].filter(x => rs.has(x)))
    xs.forEach((v) => {
      candidates.add(v)
    })
  })

  return Array.from(candidates.values())
    .filter((node: number) => {return isCloseEnough(h, node)})
    .map((node: number) => { return {a: h.value, b: node, cost: hammingWeight(h.value ^ node)} })
}

function isCloseEnough(h: Hextet, node: number): boolean {
  const dist = hammingWeight(h.value ^ node)
  return (dist <= 2)
}


// This is an awesome algorithm. It takes advantage of what subtractin 1 does to binary numbers!
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
  return bits.reduce((acc, x, i) => acc + ((2*x) ** i))
}


console.log(computeBinaryClusteringMaxK("data/clustering_big.txt"))
