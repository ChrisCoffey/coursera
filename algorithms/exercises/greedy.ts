import * as fs from 'fs'
import {send} from 'process'
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

  return jobs.slice(1).map((job) => {
      completionTime += job.length
      return completionTime * job.weight
    }).reduce((a,b) => {return a+b})
}

function prepareJobs(dataPath: string): Job[] {
  const rawLines = fs.readFileSync(dataPath)
  const jobs : Job[] = rawLines.toString().split("\n").map(parseJob)
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
  if(left.weight > right.weight) { return -1 }
  if(left.weight < right.weight) { return 1 }
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
  //const edgeHeap = new heap.MinHeap<Edge>(compEdge)
  const seenVertices: Set<number> = new Set()
  const allEdges = parseEdges('data/GreedyMST.test')
  const vertices: Edge[][] = groupEdges(allEdges)
  const mst = []
  let smallestEdges = minEdges(vertices)

  seenVertices.add(1)
  mst.push(smallestEdges[1])

  while(seenVertices.size != vertices.length) {
    const frontier : Edge[] = []

    seenVertices.forEach((v) => {
      const xs = vertices[v].filter((edge) => {
          return !seenVertices.has(edge.b)
        })
      xs.forEach((x) => {frontier.push(x)})
    })

    const minEdge = minBy(compEdge, frontier)
    console.log(minEdge, seenVertices)
    mst.push(minEdge)
    seenVertices.add(minEdge.b)
  }

  console.log(mst)
}

function parseEdges(path: string): Edge[] {
  const raw = fs.readFileSync(path)
  const parseEdge = (ln: string) => {
    const  [a,b, weight] = ln.split(" ").map((x) => parseInt(x))
    return {a: a, b: b, weight: weight}
  }

  const edges: Edge[] = raw.toString().split("\n").map(parseEdge)
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

function minEdges(edges: Edge[][]): Edge[] {
  return edges.map((focusedEdges) => { return minBy(compEdge, focusedEdges)} )
}

function minBy<A>(tc: heap.Comparable<A>, xs: A[]): A {
  let min = xs[0]
  xs.forEach((x) => {
    if(tc(x, min) === heap.CompareResult.Lt) { min = x }
  })
  return min
}

//jobWeightedCompletions(differenceSort)
//jobWeightedCompletions(ratioSort)
primsMST()

