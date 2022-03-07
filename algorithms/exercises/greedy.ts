import * as fs from 'fs'

type Job = { weight: number, length: number }
type GreedySchedule = (left: Job, right: Job) => number

function jobWeightedCompletions(scheduleFunc: GreedySchedule): void {
  const cleanedJobs = prepareJobs('data/GreedyJobs.txt')
  const result = weightedCompletionTimes(cleanedJobs, scheduleFunc)

  console.log(result)
}

function weightedCompletionTimes(rawJobs: Job[], scheduleFunc: GreedySchedule): number {
  const jobs = rawJobs.sort(scheduleFunc).reverse()
  let completionTime: number = 0

  return jobs.slice(1).map((job) => {
      completionTime += job.length
      return completionTime * job.weight
    }).reduce((a,b) => {return a+b})
}

function prepareJobs(dataPath: string): Job[] {
  const rawLines = fs.readFileSync(dataPath)
  const jobs : Job[] = rawLines.toString().split("\n").map(parseJob)
  return jobs
}

function ratioSort(left: Job, right: Job): number {
  const priorityL = ratioPriority(left)
  const priorityR = ratioPriority(right)

  if(priorityL < priorityR) { return -1 }
  if(priorityL > priorityR) { return 1 }
  return 0
}

function ratioPriority(job: Job): number {
  return job.weight / job.length
}

function differenceSort(left: Job, right: Job) : number {
  const priorityL = differencePriority(left)
  const priorityR = differencePriority(right)

  if(priorityL < priorityR) { return -1 }
  if(priorityR < priorityL) { return 1 }
  return compareByWeight(left, right)
}

// This is the non-optiomal "difference" algorithm for determining priority
function differencePriority(job: Job): number {
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


jobWeightedCompletions(differenceSort)
jobWeightedCompletions(ratioSort)


