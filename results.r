
# Clearing environment and loading libraries and resources.
rm(list = ls())
library(ggplot2)
source("util.r")

load("./data/survey.RData") # Loading survey 


#a = s$q27$i[[1]]
#b = s$q27$i[[3]]

#TCM <- emptyTCM
#TCM <- TCM[[any(TCM)]]

# Finding and removing all empty TC-mappings. The survey question on TC-mapping
# could not be skipped: in order to proceed with the survey, in question 27
# respondents had to match least one topic with at least one criterion.
# So, all empty TC-mappings (missing responses) from q27 were from respondents
# who had left the survey. Furthermore, a few respondents did not answer
# question 27 fully: instead, they just matched the first topic with the first
# criterion, so that they could progress with the survey.
# Both these cases (empty TC-mappings and TC-mappings with only one match) are
# to be considered missing data and must be removed from our set of valid
# TC-mappings:
emptyTCM <- c()
for (i in 1:length(s$q27$i)){
  if (all(s$q27$i[[i]]) | sum(s$q27$i[[i]]) <= 1) {
    emptyTCM <- c(emptyTCM, i)
  }
}
TCM <- s$q27$i[-emptyTCM]


# For the remaining valid TC-mappings, we need to calculate the average Hamming
# distance, which operationalizes our independent variable, TC-mapping
# diversity.
# So, we create a dataframe "diff" where each rows represent a unique pair of
# TC-mappings.
diff <- as.data.frame(t(combn(1:length(TCM), m = 2)))
diff$d <- NA


# For each of these pairs, we calculate and store the Hamming distance:
diff$d <- apply(
  X = diff,
  MARGIN = 1,
  FUN = function(x) twdis(a = TCM[[x[1]]], b = TCM[[x[2]]], normalized = TRUE)
)

# The average dissimilarity between TC-mappings is the average of these
# differences:
mean(diff$d)

#------------

# Now that we have this measurement, we want to understand if (and by how much)
# TC-mappings from the survey responses are more similar to each other than they
# are to random TC-mappings.
# So our next question is: how much dissimilarity is there between *random*
# TC-mappings?
#
# Note that since randomness is involved, we calculate this a few times (1000)
# over different possible TC-mapping randomizations.
set.seed(20210312)
estim <- c()

pb <- txtProgressBar(min = 1, max = 100, initial = 0, style = 3)
cat("Estimating average dissimilarity between random TC-mappings\n")
setTxtProgressBar(pb, 0)
for (rep in 1:100) {
  
  # We start by creating random TC-mappings. Note that we need to keep their
  # density (i.e. the number of TC "matches") constant: this is because networks
  # of different densities are inherently more likely to be dissimilar than
  # equal-density networks. So we randomize TC-mappings while keeping constant
  # densities by simply "rewiring" the matches in the survey TC-mappings.
  # This ensures that the density distribution in randomized TC-mappings is
  # identical to that of the real-world TC-mappings we want to compare it to.
  rTCM <- list()
  for(i in 1:261) { # For all TC-mappings in the survey
    rTCM[[i]] <- matrix(
      
      # This line is what takes a TC-mapping from the survey and scrambles it:
      c(TCM[[i]])[sample(1:length(TCM[[i]]), size = length(TCM[[i]]))],
      ncol = ncol(TCM[[i]])
    )
  }
  
  # Now that we have "scrambled"/randomized the TC-mappings we can re-calculate
  # their Hamming distance.
  x <- apply(
    X = diff,
    MARGIN = 1,
    FUN = function(x) twdis(
      a = rTCM[[x[1]]],
      b = rTCM[[x[2]]],
      normalized = TRUE
    )
  )
  
  # And save the result:
  estim[rep] <- mean(x)
  setTxtProgressBar(pb, rep)
}
close(pb)

# Let's see what the average difference between random TC-mappings is:
hist(estim)
summary(estim)
mean(estim)
# We now have our benchmark :)
