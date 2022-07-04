import * as fs from 'fs'

type Problem = {knapsackSize: number, items: Item[]}
type Item = {weight: number, value: number}

function parse_knapsack(path: string): Problem {
  const fileBuffer = fs.readFileSync(path)
  const rawLines = fileBuffer.toString().split("\n")
  const pairs : number[][] = rawLines.map((txt) => txt.split(" ").map((str) => parseInt(str)))


  const header: number[] = pairs.shift() || [0, 0]
  pairs.pop() // There's a trailing newline that gives garbage results

  const items: Item[] = pairs.map( (raw) => {
    return {weight: raw[1], value: raw[0]}
  })

  return {
    knapsackSize:  header[0],
    items: items
  }
}


console.log(parse_knapsack("data/knapsack.txt"))
