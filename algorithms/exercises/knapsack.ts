import * as fs from 'fs'

type Problem = {knapsackSize: number, items: Item[]}
type Item = {weight: number, value: number}

function iterative_knapsack(problem: Problem): number {
  const A: number[][]  = []
  for(let i = 0; i < problem.items.length; i++) {
    A[i] = new Array(problem.knapsackSize)
  }

  // Populate the first row and column with 0
  for(let x = 0; x <= problem.knapsackSize; x++) { A[0][x] = 0; }
  for(let i = 0; i < problem.items.length; i++) { A[i][0] = 0 }

  // Index into the items from 1, not 0. This is only to make things simpler with tracking initial
  // values for the weights
  for(let i = 1; i < problem.items.length; i++) {
    for(let w = 0; w <= problem.knapsackSize; w++) {
      if (problem.items[i].weight > w) {
        // When the item's weight is larger than possible capacity at this point, use the
        // prior max-value because the current item is ineligible
        A[i][w] = A[i-1][w]
      } else {
        const prev = A[i-1][w]
        const including_current = A[i-1][w - problem.items[i].weight] + problem.items[i].value

        A[i][w] = Math.max(prev, including_current)
      }
    }
  }

  return A[problem.items.length-1][problem.knapsackSize]
}

function item_centric_knapsack(problem: Problem): number {
  const seen: number[] = []

  // Iterate over items, adding each one into arr, and adding the item to any
  // previously seen items in arr, storing the new value at a smaller weight
  // Should be O(n^2), but consume less memory
  // Basically produces all permutations of items that could fit in knapsack, then
  // walk over that list in reverse order and take the max value closes to the knapsackSize


  for (let i = 1; i < problem.items.length; i++) {
    const item = problem.items[i]

    if(item.weight > problem.knapsackSize) { continue }


    const updates : number[] = []
    seen.forEach((seenValue, seenWeight) => {
      const wPrime = seenWeight + item.weight
      const vPrime = seenValue + item.value

      if(wPrime <= problem.knapsackSize && (seen[wPrime] || 0) < vPrime) {
        updates[wPrime] = vPrime
      }

    })

    updates.forEach((x,i) =>{ seen[i] = x })

    const x = seen[item.weight]
    if( x === undefined) { seen[item.weight] = item.value }
  }

  let maximum = 0
  seen.forEach((x) => { if(x > maximum) { maximum = x} })

  return maximum
}

function parse_knapsack(path: string): Problem {
  const fileBuffer = fs.readFileSync(path)
  const rawLines = fileBuffer.toString().split("\n")
  const pairs : number[][] = rawLines.map((txt) => txt.split(" ").map((str) => parseInt(str)))


  const header: number[] = pairs[0]
  pairs.pop() // There's a trailing newline that gives garbage results

  const items: Item[] = pairs.map( (raw) => {
    return {
      value: raw[0],
      weight: raw[1]
    }
  })

  return {
    knapsackSize:  header[0],
    items: items
  }
}


const data_1 = parse_knapsack("data/knapsack.txt")
console.log(iterative_knapsack(data_1))

const data_2 = parse_knapsack("data/knapsack_big.txt")
console.log(item_centric_knapsack(data_2))
