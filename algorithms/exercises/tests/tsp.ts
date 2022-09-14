import { expect } from 'chai'
import * as tsp from '../tsp'

describe("Sample test", function () {
  it("ads two numbers", function() {
    expect(2+2).to.equal(4)
  })
})

describe("TSP#numSetBits()", function(){
  it("returns 1 for powers of two", function() {
    for(let pow=0; pow <= 10; pow++){
      const n = 2 ** pow
      expect(tsp.numSetBits(n)).to.equal(1)
    }
  })

  it("returns 0 for 0", function() {
    expect(tsp.numSetBits(0)).to.equal(0)
  })

  it("properly computes for non powers-of-two", function(){
    for(let pow=0; pow<=10; pow++) {
      const n = (2**pow) - 1
      expect(tsp.numSetBits(n)).to.equal(pow)
    }
  })
})

describe("TSP#subsets", function() {
  const xs = [1,2,3,4,5,6]
  it("returns n subsets when size is 1", function() {
    expect(tsp.subsets(xs, 1).length).to.equal(xs.length)
  })

  it("returns 1 subset when size is n", function() {
    expect(tsp.subsets(xs, xs.length).length).to.equal(1)
  })

  it("returns (n choose k) subsets", function(){
    for(let k=1; k < xs.length; k++) {
      const chooseK = tsp.factorial(xs.length) / (tsp.factorial(k) * tsp.factorial(xs.length - k))
      expect(tsp.subsets(xs, k).length).to.equal(chooseK)
    }
  })
})

describe("TSP#perimeter", function() {
  it("returns 0 for an empty array", function() {
    expect(tsp.perimeter([])).to.equal(0)
  })

  it("returns 0 for an array of length 1", function() {
    expect(tsp.perimeter([{x: 1, y: 1}])).to.equal(0)
  })

  it("returns perimeter for a simple shape", function() {
    const points = [
      {x: 0, y: 0},
      {x: 0, y: 1},
      {x: 1, y: 1},
      {x: 1, y: 0}
    ]

    expect(tsp.perimeter(points)).to.equal(4)
  })
})

describe("TSP#findAngle", function() {
  it("returns 0 for horizontal line", function () {
    expect(tsp.findAngle({x: 0, y: 0}, {x: 1, y: 0})).to.equal(0)
  })

  it("Handles very similar angles properly", function() {
    const p = {x: 27166.6667, y: 9833.3333}
    const a = {x: 27233.3333, y: 10450}

    expect(Math.floor(tsp.findAngle(p, a) * 1000)).to.equal(83829)
  })
})

describe("TSP#grahamScan", function() {
  it("solves case 1", function() {
    const points = [
      {x: 1, y: 1},
      {x: 0, y: 1},
      {x: 0, y: 0},
      {x: 1, y: 0},
      {x: 0.5, y: 0.1}
    ]

    const hull = tsp.grahamScan(points)
    expect(tsp.perimeter(hull)).to.equal(4)
  })


})
