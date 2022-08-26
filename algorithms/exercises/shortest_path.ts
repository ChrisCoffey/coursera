import * as fs from 'fs'
import * as heap from './heap'

type Vertex = number
type WeightedEdge = {start: Vertex, end: Vertex, weight: number}

type Graph = {
  vertices: Vertex[],
  edges: WeightedEdge[],
  outboundEdges: WeightedEdge[][],
  inboundEdges: WeightedEdge[][]
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

  const distances: number[] = []
  distances[source] = 0

  //compute distnace based on the shortest path from `source`
  const dist = (e: WeightedEdge) => { return e.weight + distances[e.start]}
  const compareEdges = (l: WeightedEdge, r: WeightedEdge) => {
    if(dist(l) < dist(r)) { return heap.CompareResult.Lt }
    if(dist(l) > dist(r)) { return heap.CompareResult.Gt }
    else return heap.CompareResult.Eq
  }

  const frontier = new heap.MinHeap(compareEdges)
  graph.outboundEdges[source].forEach((e) => frontier.insert(e) )

  while(frontier.size() > 0) {
    // Find the min crossing edge
    const edge = frontier.extractMin()
    if(edge === undefined) { console.log("Heap is empty!"); return undefined }

    // Discard edges to vertices that already have shortest paths. This should be
    // faster than pruning the heap
    if(distances[edge.end] && distances[edge.end] < dist(edge)) { continue }

    // Update the frontier
    graph.outboundEdges[edge.end]
      .filter((e) => {return distances[e.end] === undefined} )
      .forEach((e) => frontier.insert(e))

    // Update the known edges
    distances[edge.end] = distances[edge.start] + edge.weight
  }

  return {
    source: source,
    paths: distances
  }
}

function johnsons(graph: Graph): SingleSourceShortestPaths[] {
  // Add an artificial vertex to each graph with a 0-weight edge to all other vertices
  // Run Bellman-Ford to calculate shortest paths from artificial node to all vertices
  // These shortest paths become vertex weights
  // Adjust all edges by their vertex weights to make them positive
  // Run Djikstras from each vertex
  // Balance out the edge adjustments to restore the original path weights

  return []
}

function floydWarshall(graph: Graph): SingleSourceShortestPaths[] | undefined {
  // Convert to adjacency matrix representation to save space & speed up code
  let adjacencyMatrix : number[][] = []
  for(let n = 0; n < graph.vertices.length; n++) {
    adjacencyMatrix[n] = new Array(graph.vertices.length).fill(Infinity)
  }
  graph.edges.forEach((e) => {
    adjacencyMatrix[e.start][e.end] = e.weight
  })
  graph.vertices.forEach((v) => {
    adjacencyMatrix[v][v] = 0
  })

  // Considers paths through an intermediate vertex, which must by definition be shortest
  for(let k = 0; k < graph.vertices.length; k++) {
    for(let i= 0; i < graph.vertices.length; i++) {
      for(let j = 0; j < graph.vertices.length; j++) {
        const distanceThroughK = adjacencyMatrix[i][k] + adjacencyMatrix[k][j]
        if(distanceThroughK < adjacencyMatrix[i][j]) {
          adjacencyMatrix[i][j] = distanceThroughK
        }
      }
    }
  }

  // convert back from adjacency matrix
  const result = adjacencyMatrix.map((weights, source) => {
    return {
      source: source,
      paths: weights
    }
  })

  // Detect negative cycle
  for(let n=0; n < graph.vertices.length; n++) {
    if(adjacencyMatrix[n][n] < 0 ) { return undefined }
  }

  return result
}


function loadGraph(path: string): Graph {
  const rawLines = fs.readFileSync(path).toString().split("\n")
  rawLines.shift()
  const edges: WeightedEdge[] = rawLines.map((str) => {
    const s = str.split(" ")
    return {start: parseInt(s[0]), end: parseInt(s[1]), weight: parseInt(s[2]) }
  })
  edges.pop()

  const inbound: WeightedEdge[][] = []
  const outbound: WeightedEdge[][] = []
  const vertices: Vertex[] = []
  edges.forEach((edge) => {
    const inb = (inbound[edge.end] || [])
    inb.push(edge)
    inbound[edge.end] = inb

    const out = (outbound[edge.start] || [])
    out.push(edge)
    outbound[edge.start] = out

    vertices.push(edge.start, edge.end)
  })

  return {
    vertices: [...new Set<Vertex>(vertices)],
    inboundEdges: inbound,
    outboundEdges: outbound,
    edges: edges
  }
}



const testGraph = loadGraph("data/g_test.txt")

const positiveTestGraph = loadGraph("data/g_test_pos.txt")
console.log(positiveTestGraph)

console.log(bellmanFord(0, testGraph))
console.log(bellmanFord(0, positiveTestGraph))
console.log(djikstras(0, positiveTestGraph))

console.log(floydWarshall(testGraph))
