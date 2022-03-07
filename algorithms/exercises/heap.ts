class MinHeap {
  private storage: number[] = []

  public extractMin(): number | undefined {
    if(this.storage.length === 0) { return undefined }

    const min = this.storage[0]
    this.rebalance()
    return min
  }

  public insert(x: number) : void {

  }

  private rebalance(): void {

  }
}

export { MinHeap }
