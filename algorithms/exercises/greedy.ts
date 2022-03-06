import * as fs from 'fs'

type Job = { weight: number, length: number }

function jobs(): void {
  const rawLines = fs.readFileSync('data/GreedyJobs.txt')
  const jobs : Job[] = rawLines.toString().split("\n").map(parseJob)


  let completionTime: number = 0
  // where is the NaN job coming from? It's at the head of the list
  const cleanedJobs = prepareJobs(jobs)
  console.log(cleanedJobs.slice(0, 10), cleanedJobs.length)
  const result = cleanedJobs.slice(1).map((job) => {
    if (job.length === NaN) { return 0 }
    completionTime += job.length
    return completionTime * job.weight
  }).reduce((a,b) => {return a+b})

  console.log(result)
}

function prepareJobs(jobs: Job[]): Job[] {
  return jobs.sort(sortByPriority).reverse()
}

function sortByPriority(left: Job, right: Job) : number {
  const priorityL = jobPriority(left)
  const priorityR = jobPriority(right)

  if(priorityL < priorityR) { return -1 }
  if(priorityR < priorityL) { return 1 }
  return compareByWeight(left, right)
}

// This is the non-optiomal "difference" algorithm for determining priority
function jobPriority(job: Job): number {
  return job.weight - job.length
}

function compareByWeight(left: Job, right: Job): number{
  if(left.weight > right.weight) { return -1 }
  if(left.weight < right.weight) { return 1 }
  return 0
}

//TODO This can fail if elems lacks a space...
function parseJob(str: string): Job {
  const elems = str.split(" ")

  return {weight: parseInt(elems[0]), length: parseInt(elems[1])}
}


jobs()
