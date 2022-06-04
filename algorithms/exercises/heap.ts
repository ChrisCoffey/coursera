enum CompareResult { Lt, Gt, Eq }

interface Comparable<A> {
  (l: A, r: A): CompareResult
}

class MinHeap<A> {
  private storage: A[] = []
  private compare: Comparable<A>

  constructor(compare: Comparable<A>) {
    this.compare = compare
  }

  public size(): number {
    return this.storage.length
  }

  public extractMin(): A | undefined {
    if(this.storage.length === 0) { return undefined }

    const min = this.storage[0]

    this.moveLastToRoot()
    if(this.storage.length > 0) {
      this.pushDown(this.storage[0], 0)
    }

    return min
  }

  public findMin(): A | undefined {
    if(this.storage.length == 0) { return undefined}

    return this.storage[0]
  }

  public delete(x: A): void {
    const idx = this.storage.indexOf(x)
    if (idx < 0) return

    this.moveLastTo(idx)
    this.pushDown(this.storage[idx], idx)
  }

  public insert(x: A) : void {
    this.pushUp(x, this.storage.length)
  }

  private pushUp(x: A, index: number): void {
    if(index === 0) { this.storage[index] = x }

    let idx = index
    let parentNode = Math.floor(idx/ 2)

    while(this.compare(x, this.storage[parentNode]) === CompareResult.Lt) {
      this.storage[idx] = this.storage[parentNode]
      idx = parentNode
      parentNode = Math.floor(parentNode / 2)

      if (idx === 0) { break }
    }

    this.storage[idx] = x
  }

  private pushDown(x: A, index: number): void {
    const leftChild = this.left(index)
    const rightChild = this.right(index)
    const smallestChild = this.storage[leftChild] < this.storage[rightChild] ? leftChild : rightChild

    if(this.leafNode(index)){
      this.storage[index] = x
    }
    else if (leftChild == this.storage.length){
      if(this.storage[leftChild] < x){
        this.storage[index] = this.storage[leftChild]
        this.storage[leftChild] = x
      }
      else {
        this.storage[index] = x
      }
    }
    else { // internal node
      if(this.storage[smallestChild] < x) {
        this.storage[index] = this.storage[smallestChild]
        this.pushDown(x, smallestChild)
      }
      else {
        this.storage[index] = x
      }
    }
  }

  private moveLastToRoot(): void {
    this.moveLastTo(0)
  }

  private moveLastTo(idx: number): void {
    this.storage[idx] = this.storage[this.storage.length -1]
    this.storage.pop()
  }

  private left(index: number): number { return (index * 2) + 1 }
  private right(index: number): number { return (index * 2) + 2 }
  private leafNode(index: number): boolean {
    return ((2 * index) + 1) > this.storage.length
  }
}

export { MinHeap, Comparable, CompareResult }
