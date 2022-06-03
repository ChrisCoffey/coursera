import * as fs from 'fs'
import './heap'

function readSymbolWeights(path: string): number[] {
  const rawText = fs.readFileSync(path)
  const rawLines = rawText.toString().split("\n")
  rawLines.shift()
  return rawLines.map(parseInt)
}
