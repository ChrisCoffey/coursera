import * as fs from 'fs'

// an SCC approach
//
// Each variable has a true and false vertex
// Each clause turns into two edges between variables
// (a || -b) => (-a, -b), (b, a)

// Each problem file is 100k clauses long. Should be solvable
// in O(m+n)

// A directed graph of the 2SAT clauses.
// Variables are the vertices and edges indicate logical implication
class SATGraph {
  private n: number
  private numVariables: number
  private graph: number[][]
  private graphT: number[][]

  constructor(numVariables: number) {
    this.n = (2 * numVariables) + 1
    this.numVariables = numVariables
    this.graph = []
    this.graphT = []

    for(let i=0; i <= this.n; i++) {
      this.graph[i] = []
      this.graphT[i] = []
    }
  }

  // TODO assign a getter?
  public size(): number { return this.n }
  public numVars(): number { return this.numVariables }

  public edgesFrom(v: number): number[] {
    return this.graph[v]
  }

  public edgesIn(v: number): number[]{
    return this.graphT[v]
  }

  public addEdges(startVar: number, endVar: number): void {
    this.graph[this.notV(startVar)].push( this.varIndex(endVar) )
    this.graph[this.notV(endVar)].push( this.varIndex(startVar) )

    // Assign the transpose
    this.graphT[this.varIndex(startVar)].push( this.notV(endVar) )
    this.graph[this.varIndex(endVar)].push( this.notV(startVar) )
  }

  // Being > 0 means v is true
  private notV(v: number): number {
    return v > 0 ? (this.varIndex(v) + this.numVariables) : (this.varIndex(v) - this.numVariables)
  }

  private varIndex(v: number): number {
    return v > 0 ? v : (Math.abs(v) + this.n)
  }
}

function isSatisfiable(graph: SATGraph): boolean {
  const seen: boolean[] = []
  const components: number[] = []
  const stack: number[] = []

  for(let i = 0; i< graph.size(); i++) {
    seen[i] = false
    components[i] = -1
  }

  // DFS on the graph, pushing verdices onto the stack
  // as they're seen
  function firstPass(v: number): void {
    seen[v] = true
    graph.edgesFrom(v).forEach((e) => {
      if(!seen[e]) {
        firstPass(e)
      }
    })
    stack.push(v)
  }

  function secondPass(v: number, componentNum: number): void {
    components[v] = componentNum
    graph.edgesIn(v).forEach((e) => {
      if(components[e] == -1) {
        secondPass(e, componentNum)
      }
    })
  }

  // Perform the first pass, starting with an arbitrary vertex
  for(let i=0; i < graph.size(); i++) {
    if(!seen[i]) {
      firstPass(i)
    }
  }

  let componentNumber = 0
  for(let i=0; i < graph.size(); i++) {
    let vertex = stack.pop()
    // If this vertex hasn't been assigned a component, assign one
    if(components[vertex] == -1) {
      secondPass(vertex, componentNumber++)
    }
  }


  return false
}



function parseSATFile(file: fs.PathLike): SATGraph {
  const rawLines: string[] = fs.readFileSync(file).toString().split("\n")
  const numLines = parseInt(rawLines.shift() || "100000")

  // Initialize the graph
  const graph: SATGraph = new SATGraph(numLines)

  rawLines.forEach((line) => {
    parseClause(graph, line)
  })

  return graph
}

// These functions are somewhat messy. Consider creating a class for them
function parseClause(graph: SATGraph, rawClause: string): void {
  const [a, b] = rawClause.split(" ").map(parseInt)
  graph.addEdges(a, b)
}


