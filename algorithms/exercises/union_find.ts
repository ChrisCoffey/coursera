// A union-find that supports path compression, for very fast find and union
// operations.
type Node<A> = { value: A, rank: number, parent: Node<A> | undefined }

class UnionFind<A> {
  private storage : Map<A, Node<A>>

  // It is not possible to add new nodes after construction
  constructor(data: A[]) {
    this.storage = new Map()
    data.forEach((d) => { this.storage.set(d, {value: d, rank: 0, parent: undefined }) } )
  }

  // with path compression
  public find(x: A): A  {
    // A root node
    const node = this.storage.get(x)
    if(node === undefined || node.parent === undefined) { return x }

    // Points directly to a root
    const p = node.parent
    if(p.parent === undefined) { return p.value }

    // compress the path recursively by repointing to the cluster root
    const rootValue: A = this.find(p.value)
    node.parent = this.storage.get(rootValue)


    return rootValue
  }

  public union(x: A, y: A): boolean {
    const xCluster = this.find(x)
    const yCluster = this.find(y)
    if (xCluster === yCluster) { return false }

    // Determine which cluster merges into the other. This is union-by-rank
    const xNode = this.storage.get(xCluster)
    const yNode = this.storage.get(yCluster)
    if(xNode === undefined || yNode === undefined) {
      console.log("[Error] unknown cluster", xCluster, yCluster, xNode, yNode)
      return false
    }

    const largerCluster = xNode.rank >= yNode.rank ? xNode : yNode
    const smallerCluster = xNode == largerCluster ? yNode : xNode

    // Merge the smaller cluster into the larger, based on rank
    smallerCluster.parent = largerCluster

    // Increment rank of the larger cluster iff both clusters had same rank
    if (smallerCluster.rank === largerCluster.rank) { largerCluster.rank += 1 }

    return true
  }

  public prettyPrint(): void {
    const clusters : Map<A, A[]> = new Map()
    for (let [_, node] of this.storage) {
        const cluster = this.find(node.value)
        const followers : A[] = clusters.get(cluster) || []
        followers.push(node.value)
        clusters.set(cluster, followers)
    }

    for(let [cluster, elements] of clusters) {
      console.log(`Cluster ${cluster} with elements ${elements}` )
    }
  }

}

export { UnionFind }
