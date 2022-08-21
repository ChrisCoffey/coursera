import * as fs from 'fs'
import * as heap from './heap'

type Vertex = number
type WeightedEdge = {start: Vertex, end: Vertex, weight: number}
interface Edges {
  [index: Vertex]: WeightedEdge[]
}

type Graph = {
  vertices: Vertex[],
  edges: WeightedEdge[],
  outboundEdges: Edges,
  inboundEdges: Edges
}

type SingleSourceShortestPaths = {
  source: Vertex,
  paths:  number[]
}

function bellmanFord(source: Vertex, graph: Graph): SingleSourceShortestPaths | undefined {
  const predecessors: Vertex[] = []
  const pathLengths: number[] = []

  graph.vertices.forEach((v) => {
    predecessors[v] = Number.NaN
    pathLengths[v] = Number.POSITIVE_INFINITY
  })
  // Set source length to 0 b/c it was just set to infinity
  pathLengths[source] = 0

  for(let i = 1; i < graph.vertices.length - 1; i++){
    graph.edges.forEach((edge) => {
      if(pathLengths[edge.end] > pathLengths[edge.start] + edge.weight) {
        pathLengths[edge.end] = pathLengths[edge.start] + edge.weight
        predecessors[edge.end] = edge.start
      }
    })
  }

  // In the case this comparison is true, an additional iteration of the algorithm would
  // reduce the distance for this path. But because the entire vertex budget has been used,
  // that means there must be a negative cycle in the graph. Hence, return undefiend to
  // indicate that a shortest path cannot exist on this graph.
  let negativeCycle = false
  graph.edges.forEach((edge) => {
    if(pathLengths[edge.end] > pathLengths[edge.start] + edge.weight) {
      negativeCycle = true
    }
  })
  if(negativeCycle) { return undefined }

  return {
    source: source,
    paths: pathLengths
  }
}

function djikstras(source: Vertex, graph: Graph): SingleSourceShortestPaths | undefined {
  let negativeEdge = false
  graph.edges.forEach((edge) => { if(edge.weight < 0) {negativeEdge = true} })
  if(negativeEdge) { return undefined }



  return {
    source: source,
    paths: []
  }
}


function loadGraph(path: string): Graph {

}



const testGraph = {
  vertices: [0,1,2,3,4],
  edges: [
    {start: 0, end: 1, weight: 6},
    {start: 0, end: 4, weight: 7},
    {start: 1, end: 4, weight: 8},
    {start: 1, end: 2, weight: 5},
    {start: 1, end: 3, weight: -4},
    {start: 2, end: 1, weight: -2},
    {start: 3, end: 2, weight: 7},
    {start: 3, end: 0, weight: 2},
    {start: 4, end: 2, weight: -3},
    {start: 4, end: 3, weight: 9},
  ],
  outboundEdges: {},
  inboundEdges: {},
}

const positiveTestGraph = {
  vertices: [0,1,2,3,4],
  edges: [
    {start: 0, end: 1, weight: 6},
    {start: 0, end: 4, weight: 7},
    {start: 1, end: 4, weight: 8},
    {start: 1, end: 2, weight: 5},
    {start: 1, end: 3, weight: 4},
    {start: 2, end: 1, weight: 2},
    {start: 3, end: 2, weight: 7},
    {start: 3, end: 0, weight: 2},
    {start: 4, end: 2, weight: 3},
    {start: 4, end: 3, weight: 9},
  ],
  outboundEdges: {},
  inboundEdges: {},
}

console.log(bellmanFord(0, testGraph))
console.log(bellmanFord(0, positiveTestGraph))
