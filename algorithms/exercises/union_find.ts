// A union-find that supports path compression, for very fast find and union
// operations.
type Node<A> = { value: A, rank: number, parent: Node<A> | null }

class UnionFind<A> {
  private storage : Map<A, Node<A>>

  private toNode = (a : A) => { return {value: a, rank: 0, parent: null } }

  // It is not possible to add new nodes after construction
  constructor(data: [A]) {
    data.forEach((d) => { this.storage.set(d, this.toNode(d)) } )
  }

  // with path compression
  public find(x: A): A  {
    // A root node
    const node = this.storage.get(x)
    if(node.parent === null) { return x }

    // Points directly to a root
    const p = node.parent
    if(p.parent === null) { return p.value }

    // compress the path recursively by repointing to the cluster root
    const rootValue: A = this.find(node.parent.value)
    node.parent = this.storage.get(rootValue)


    return node.parent.value
  }

  public union(x: A, y: A): void {
    const xCluster = this.find(x)
    const yCluster = this.find(y)
    if (xCluster === yCluster) { return }

    // Determine which cluster merges into the other
    const xNode = this.storage.get(xCluster)
    const yNode = this.storage.get(yCluster)

    const largerCluster = xNode.rank > yNode.rank ? xNode : yNode
    const smallerCluster = xNode == largerCluster ? yNode : xNode

    // Merge the smaller cluster into the larger, based on rank
    smallerCluster.parent = largerCluster

    // Increment rank of the larger cluster iff both clusters had same rank
    if (smallerCluster.rank === largerCluster.rank) { largerCluster.rank += 1 }

  }

}

export { UnionFind }
