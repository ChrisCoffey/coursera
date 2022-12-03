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

  public addEdges(a: number, negateA: number, b: number, negateB: number): void {
    const ia = 2 * a ^ negateA
    const ib = 2 * b ^ negateB
    const negA = ia ^ 1
    const negB = ib ^ 1
    //initialize the edges if they haven't been yet
    if(this.graph[negA] === undefined ) { this.graph[negA] = [] }
    if(this.graph[negB] === undefined ) { this.graph[negB] = [] }
    if(this.graphT[ia] === undefined ) { this.graphT[ia] = [] }
    if(this.graphT[ib] === undefined ) { this.graphT[ib] = [] }

    this.graph[negA].push(ib)
    this.graph[negB].push(ia)
    this.graphT[ib].push(negA)
    this.graphT[ia].push(negB)
  }
}

function isSatisfiable(graph: SATGraph): [boolean, boolean[]] {
  const seen: boolean[] = []
  const components: number[] = []
  const stack: number[] = []
  const assignment: boolean[] = []

  for(let i = 0; i< graph.size(); i++) {
    seen[i] = false
    components[i] = -1
  }

  // DFS on the graph, pushing verdices onto the stack
  // as they're seen
  function firstPass(v: number): void {
    seen[v] = true
    //console.log(v, graph, seen, stack)
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
    if (vertex === undefined) {
      console.log("ERROR, stack prematurely empty")
      return [false, []]
    }
    // If this vertex hasn't been assigned a component, assign one
    if(components[vertex] == -1) {
      secondPass(vertex, componentNumber++)
    }
  }

  //TODO calcualte assignments
  for(let i=0; i< graph.numVars(); i += 2) {
    if(components[i] == components[i+1]) {
      return [false, []]
    }
    assignment[Math.floor(i / 2)] = components[i] > components[i+1]
  }

  return [true, assignment]
}


function parseSATFile(file: fs.PathLike): SATGraph {
  const rawLines: string[] = fs.readFileSync(file).toString().split("\n")
  const numLines = parseInt(rawLines.shift() || "100000")
  rawLines.pop()
  const numVariables = calculateNumVars(rawLines)

  // Initialize the graph
  const graph: SATGraph = new SATGraph(numVariables)


  rawLines.forEach((line) => {
    parseClause(graph, line)
  })

  return graph
}

// These functions are somewhat messy. Consider creating a class for them
function parseClause(graph: SATGraph, rawClause: string): void {
  const [a, b] = rawClause.split(" ").map( (x) => parseInt(x))
  const negateA = a < 0 ? 1 : 0
  const negateB = b < 0 ? 1 : 0
  graph.addEdges(Math.abs(a), negateA, Math.abs(b), negateB)
}

function calculateNumVars(lines: string[]): number {
  let largestVarNum = -1
  lines.forEach((l) => {
    const [a, b] = l.split(" ").map( (x) => parseInt(x) )
    largestVarNum = Math.max(largestVarNum, Math.abs(a), Math.abs(b))
  })

  return largestVarNum
}

const files = [
  "data/2sat.test1",
  "data/2sat.test_2",
  "data/2sat.test3",
  "data/2sat.test4",
  "data/2sat.test5"
]
//const files = ["data/2sat.test3"]

files.forEach((f) => {
  const graph = parseSATFile(f)
  const satisfied = isSatisfiable(graph)[0]
  console.log(f, satisfied)
})
