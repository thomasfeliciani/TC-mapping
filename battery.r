# This script runs a battery of simulations to explore the specified 
# parameter space. Sourcing this script is not advised, as it will take 
# several hours to complete the task.

# Cleaning environment and loading resources:
rm (list = ls( ))
source("simulation.r")
library("compiler")
library("parallel")
library("doSNOW")
load("./data/pTCM.RData") # This contains the weights for a TC-mapping template.


nRepetitions <- 2
#nRepetitions <- 1 # repetitions per execution


battery <- expand.grid(
  nReviewers = 5,
  nProposals = 10,
  attributeMean = 0.75,
  attributeSD = c(0.1, 0.5),
  attributeCorr = c(0, 0.5),
  nTopics = c(5, 10, 12, 15, 20),
  nCriteria = 2:5,
  reviewerNoise = c(0, 0.1, 0.2),
  GLdiversity = c(0, 0.1, 0.2),
  gradingScale = c(2, 5, 10),
  TCMswapping = 0.1 * 0:5
)
#battery <- battery[51:100,]##############################

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
        reviewerNoise = as.numeric(x["reviewerNoise"]),
        GLdiversity = as.numeric(x["GLdiversity"]),
        gradingScale = as.numeric(x["gradingScale"]),
        TCMswapping = as.numeric(x["TCMswapping"]),
        pTCM = pTCM
      )$summary
    }
  ))
  
  return(as.data.frame(sim))
}



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





# Saving results to file________________________________________________________
save(
  file = "./output/ri.RData",
  ri, battery
)

print(object.size(ri), units="Mb")

