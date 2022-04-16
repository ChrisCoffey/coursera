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

  while(numClusters > k) {
    if(sortedEdges.length == 0) { break }
    const edge: Edge = sortedEdges[0]
    const c1 = unionFind.find(edge.a)
    const c2 = unionFind.find(edge.b)

    if(c1 !== c2) {
      unionFind.union(c1, c2)
      numClusters--
    }

    sortedEdges.shift()
  }


  unionFind.prettyPrint()
  return sortedEdges[0].cost
}

function processEdgeFile(path: string): Edge[] {
  const rawLines = fs.readFileSync(path).toString().split("\n").filter(x => x.length > 0)
  const edges = rawLines.map(l => {
    const [a,b,cost, ..._]: string[] = l.split(" ")
    return {a: parseInt(a), b: parseInt(b), cost: parseInt(cost)}
  })
  return edges
}




console.log(maxSpacingKClustering("data/clustering1.test", 2))
