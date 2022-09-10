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

