# This script runs a battery of simulations to explore the specified 
# parameter space. Sourcing this script is not advised, as it will take 
# several hours to complete the task.

# ______________________________________________________________________________
# Cleaning environment and loading resources.

rm (list = ls())
source("simulation.r")
library("compiler")
library("parallel")
library("doSNOW")
load("./data/pTCM.RData") # This contains the weights for a TC-mapping template.


#pTCM[1:12,1:3] <- 0.5 ######## (for extra analyses to test homogeneous mapping)

# ______________________________________________________________________________
# Defining the parameter space to be explored. 

nRepetitions <- 500 # repetitions per execution. Default is 500

battery <- expand.grid(
  nReviewers = c(3, 5, 10),
  nProposals = 10,
  attributeMean = 0.75,
  attributeSD = 0.2,
  attributeCorr = c(0, 0.5),
  nTopics = c(6, 12, 24),
  nCriteria = c(2, 3, 5),
  reviewerError = c(0, 0.1, 0.2),
  reviewerBiasDiversity = c(0, 0.1, 0.2),
  #reviewerNoise = c(0, 0.1, 0.2),
  GLdiversity = c(0, 0.1, 0.2),
  gradingScale = c(2, 5, 10),
  TCMswapping = c(0, 0.05, 0.1, 0.2, 0.4)
)


# Removing parameter configurations that differ from the baseline from more than
# two dimensions. This considerably reduces the parameter space that needs to 
# be simulated.
# To do this, we first define a baseline parameter configuration:
baseline = c(
  nReviewers = 3,
  nProposals = 10,
  attributeMean = 0.75,
  attributeSD = 0.2,
  attributeCorr = 0.5,
  nTopics = 12,
  nCriteria = 3,
  reviewerError = 0.1,
  reviewerBiasDiversity = 0.1,
  GLdiversity = 0.1,
  gradingScale = 5#,
  #TCMswapping = 0.1 * 0:5
)

# Then, for each parameter configuration in the data.frame battery, we count
# how many differences there are, and we discard runs that have more than 3
# differences.
variables <- c(
  "nReviewers", "nProposals", "attributeMean", "attributeSD", "attributeCorr",
  "nTopics", "nCriteria", "reviewerError", "reviewerBiasDiversity",
  "GLdiversity", "gradingScale")
closeToBaseline <- apply(
  X = battery,
  MARGIN = 1,
  FUN = function(x) {
    nDiff <- sum(x[variables] != baseline) # tally of differences from baseline
    ifelse(nDiff > 3, return(FALSE), return(TRUE))
  }
)
battery <- battery[closeToBaseline,]
#battery <- battery[51:100,] # This shortens the battery further (for testing).



# ______________________________________________________________________________
# Running simulation batteries.
#
# Next, we define how to simulate a single battery. "Battery" here refers to a
# single parameter configuration, i.e. a single line from the "battery"
# data.frame.
runBattery <- function(battery) {
  
  battery$seed <- sample(
    -99999999:99999999,
    size = nrow(battery),
    replace = FALSE
  )
  
  sim <- t(apply(
    X = battery,
    MARGIN = 1,
    FUN = function(x) {
      print(x)
      simulation(
        seed = as.numeric(x["seed"]),
        nReviewers = as.numeric(x["nReviewers"]),
        nProposals = as.numeric(x["nProposals"]),
        attributeMean = as.numeric(x["attributeMean"]),
        attributeSD = as.numeric(x["attributeSD"]),
        attributeCorr = as.numeric(x["attributeCorr"]),
        nTopics = as.numeric(x["nTopics"]),
        nCriteria = as.numeric(x["nCriteria"]),
        reviewerError = as.numeric(x["reviewerError"]),
        reviewerBiasDiversity = as.numeric(x["reviewerBiasDiversity"]),
        #reviewerNoise = as.numeric(x["reviewerNoise"]),
        GLdiversity = as.numeric(x["GLdiversity"]),
        gradingScale = as.numeric(x["gradingScale"]),
        TCMswapping = as.numeric(x["TCMswapping"]),
        pTCM = pTCM
      )$summary
    }
  ))
  
  return(as.data.frame(sim))
}


# Now we are ready to run multiple batteries in parallel.
print(paste("Simulation battery started on", Sys.time()))
enableJIT(1)
cl <- snow::makeCluster(
  parallel::detectCores() - 2,
  outfile = "./output/log.txt"
)
registerDoSNOW(cl)
pb <- txtProgressBar(max = nRepetitions, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
ri <- foreach(
  i = 1:nRepetitions,
  .combine = rbind,
  .options.snow = opts
) %dopar% runBattery(battery)
close(pb)
stopCluster(cl)
enableJIT(0)
print(paste("Simulation battery completed on", Sys.time()))




# ______________________________________________________________________________
# Saving results to file

save(
  file = "./output/ri.RData",
  #file = "./output/ri_extra.RData",
  ri, battery
)

print(object.size(ri), units = "Mb")

