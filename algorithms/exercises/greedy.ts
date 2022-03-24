import * as fs from 'fs'
import * as heap from './heap'

type Job = { weight: number, length: number }
type GreedySchedule = (left: Job, right: Job) => number

function jobWeightedCompletions(scheduleFunc: GreedySchedule): void {
  const cleanedJobs = prepareJobs('data/GreedyJobs.txt')
  const result = weightedCompletionTimes(cleanedJobs, scheduleFunc)

  console.log(result)
}

function weightedCompletionTimes(rawJobs: Job[], scheduleFunc: GreedySchedule): number {
  const jobs = rawJobs.sort(scheduleFunc).reverse()
  let completionTime: number = 0

  return jobs.map((job) => {
      completionTime += job.length
      return completionTime * job.weight
    }).reduce((a,b) => {return a+b})
}

function prepareJobs(dataPath: string): Job[] {
  const rawLines = fs.readFileSync(dataPath)
  const jobs : Job[] = rawLines.toString().split("\n").filter(x => x.length > 0).map(parseJob)
  return jobs
}

function ratioSort(left: Job, right: Job): number {
  const priorityL = ratioPriority(left)
  const priorityR = ratioPriority(right)

  if(priorityL < priorityR) { return -1 }
  if(priorityL > priorityR) { return 1 }
  return 0
}

function ratioPriority(job: Job): number {
  return job.weight / job.length
}

function differenceSort(left: Job, right: Job) : number {
  const priorityL = differencePriority(left)
  const priorityR = differencePriority(right)

  if(priorityL < priorityR) { return -1 }
  if(priorityR < priorityL) { return 1 }
  return compareByWeight(left, right)
}

// This is the non-optiomal "difference" algorithm for determining priority
function differencePriority(job: Job): number {
  return job.weight - job.length
}

function compareByWeight(left: Job, right: Job): number{
  if(left.weight < right.weight) { return -1 }
  if(left.weight > right.weight) { return 1 }
  return 0
}

//TODO This can fail if elems lacks a space...
function parseJob(str: string): Job {
  const elems = str.split(" ")

  return {weight: parseInt(elems[0]), length: parseInt(elems[1])}
}


type Edge = {a: number, b: number, weight: number}
function compEdge(l: Edge, r: Edge): heap.CompareResult {
  if (l.weight < r.weight) { return heap.CompareResult.Lt }
  if (l.weight > r.weight) { return heap.CompareResult.Gt }
  return heap.CompareResult.Eq
}

type Vertex = {a: number, weight: number}


function primsMST() {
  const seenVertices: Set<number> = new Set()
  const allEdges = parseEdges('data/GreedyGraph.txt')
  const vertices: Edge[][] = groupEdges(allEdges)
  const mst = []

  seenVertices.add(1)

  while(seenVertices.size != vertices.length - 1) {
    const newVertices = vertices.filter((edges, vertex) => {
      return edges !== undefined && !seenVertices.has(vertex)
    })

    // Create a cut between the seen and unseen vertices, returning all edges that cross it
    const frontier = newVertices.flatMap((edges) => {
      const crossingEdges = edges.filter((edge) => { return seenVertices.has(edge.b) })
      return crossingEdges.length > 0 ? [minBy(compEdge, crossingEdges)] : []
    })

    // a is not in the seenVertices, but b is
    const minEdge = minBy(compEdge, frontier)
    seenVertices.add(minEdge.a)
    mst.push(minEdge)
  }

  const mstSum = mst.map(edge => edge.weight).reduce((l,r) => l + r)
  console.log(mstSum)
}

function parseEdges(path: string): Edge[] {
  const raw = fs.readFileSync(path)
  const parseEdge = (ln: string) => {
    const  [a,b, weight] = ln.split(" ").map((x) => parseInt(x))
    return {a: a, b: b, weight: weight}
  }
  const edges: Edge[] = raw.toString().split("\n").filter((x) => x.length > 0).map(parseEdge)
  return edges
}

function groupEdges(edges: Edge[]): Edge[][] {
  const groupedEdges : Edge[][] = []
  edges.forEach((edge) => {
    if(groupedEdges[edge.a] === undefined) { groupedEdges[edge.a] = []}
    if(groupedEdges[edge.b] === undefined) { groupedEdges[edge.b] = []}

    groupedEdges[edge.a].push(edge)
    const flippedEdge = {a: edge.b, b: edge.a, weight: edge.weight}
    groupedEdges[edge.b].push(flippedEdge)
  })
  return groupedEdges
}

function minBy<A>(tc: heap.Comparable<A>, xs: A[]): A {
  let min = xs[0]
  xs.forEach((x) => {
    if(tc(x, min) === heap.CompareResult.Lt) { min = x }
  })
  return min
}

jobWeightedCompletions(differenceSort)
jobWeightedCompletions(ratioSort)
primsMST()

