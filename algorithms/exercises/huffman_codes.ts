import * as fs from 'fs'
import * as h from './heap'
require('source-map-support').install()

type SymbolNode = {indices: number[], weight: number}

function compareByWeight(l: SymbolNode, r: SymbolNode): h.CompareResult {
  if(l === undefined || r === undefined) {console.trace()}

  if(l.weight > r.weight) { return h.CompareResult.Gt }
  if(l.weight < r.weight) { return h.CompareResult.Lt }
  return h.CompareResult.Eq
}

function calculateHuffmanCodeLengths(path: string): number[] {
  console.log("starting")
  const symbolWeights = readSymbolWeights(path)
  const codeLengths: number[] = []
  const heap = new h.MinHeap<SymbolNode>(compareByWeight)
  console.log("prepared")

  symbolWeights.forEach((weight, index) => {
    heap.insert({weight: weight, indices: [index]})
    codeLengths[index] = 0
  })
  console.log("heap initialized")


  while(heap.findMin() !== undefined) {
    const min = heap.extractMin()
    const next = heap.extractMin()
    if(min === undefined || next === undefined){
      break
    }
    const merged = mergeNodes(min, next)

    heap.insert(merged)
    recordMerge(merged, codeLengths)
  }

  return codeLengths
}

function mergeNodes(l: SymbolNode, r: SymbolNode): SymbolNode {

  return {
    weight: l.weight + r.weight,
    indices: l.indices.concat(r.indices)
  }
}

function recordMerge(mergedNode: SymbolNode, counts: number[]): void {
  for (let i of mergedNode.indices) {
    counts[i] += 1
  }
}


function readSymbolWeights(path: string): number[] {
  const rawText = fs.readFileSync(path)
  const rawLines = rawText.toString().split("\n")
  rawLines.shift()
  rawLines.pop()

  return rawLines.map((x) => { return parseInt(x) })
}


console.log(calculateHuffmanCodeLengths("data/huffman_codes.txt"))
