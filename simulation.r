# This script defines the main simulation function.
# At the end of the script there is some example code showing how to run it.
rm(list=ls())

# Loading auxiliary functions and libraries:
source("./util.r")
library("faux")

seed = sample(-99999999:99999999, size = 1)
nReviewers = 5
nTopics = 5
nCriteria = 4
topicsError = rep(0.2, times = nTopics)
topicsBias = runif(n = nTopics, min = -0.2, max = 0.2)
GLinterpretation = "asymmetric"
GLdiversity = 0.2
gradingScale = 5 # number of categories in the evaluation scale
TCMdiversity = 0.2
q = 0.5
sdq = 0.3
#calibrationData = "random" # or "survey" (survey data not available on GitHub)


# Setting random seed
set.seed(seed)

# Creating a proposal with a given number of "cues"/topics of correlated
# quality
prop <- truncate(rnorm_multi(
  n = 1,
  mu = rep(q, times = nTopics),
  sd = rep(sdq, times = nTopics),
  r = 0.7,
  as.matrix = TRUE
))

# A grade language is a vector of thresholds used to discretize the cues from
# the proposals (continuous variable) into a discrete grade.
# Here we define the "standard" grade language.
GLthresholds <- c()
if (GLinterpretation == "random"){ # ... then set thresholds at random.
  GLthresholds <- runif (
    n = gradingScale - 1, # This determines how many thresholds we need
    min = 0, max = 1
  )
  GLthresholds <- GLthresholds[order(GLthresholds)]
} else if (GLinterpretation == "symmetric"){ # ... then spread them evenly.
  GLthresholds <- (1:gradingScale - 1) / gradingScale
  GLthresholds <- GLthresholds[-1]
} else if (GLinterpretation == "asymmetric"){ #...then they are left-skewed.
  for (l in 1:(gradingScale - 1)){
    GLthresholds[l] <- 1 - ((3 / 5) ^ l) 
  }
}

# We also create a "template" TC-mapping from which each reviewer's own
# mapping will deviate to a specified degree. The "template" mapping is
# created by making links between topics and criteria with a probability 
# equal to the relative frequency of that link from the survey responses.
# Unlike the raw survey microdata that cannot be shared, the probability
# weights constitute aggregate statistics and thus can be (and are) included
# in the GitHub repository.
# In our intentions, this way of creating TC-mappings allows for realistic
# mappings in the simulation while preserving full reproducibility of our
# results. It allows allows for easy manipulation of the TC-mapping network,
# specifically of its size (number of topics and of criteria) and inter-
# -reviewer diversity.
#
# Loading probability weights:
load("./data/pTCM.RData")

# Sampling (or cloning) topics and criteria (i.e. rows and columns) to create
# a template mapping of the desired size:
ifelse(
  nTopics <= nrow(pTCM),
  topics <- sort(sample(1:nrow(pTCM), size = nTopics, replace = FALSE)),
  topics <- sample(1:nrow(pTCM), size = nTopics, replace = TRUE)
)
ifelse(
  nCriteria <= ncol(pTCM),
  criteria <- sort(sample(1:ncol(pTCM), size = nCriteria, replace = FALSE)),
  criteria <- sample(1:ncol(pTCM), size = nCriteria, replace = TRUE)
)
pTCM <- pTCM[topics,criteria]

# Now we use the probability weights in pTCM to determine where the TC-links
# are in the "template" TC-mapping. We do this via Bernoulli trials.
TCM <- matrix(
  rbinom(nTopics * nCriteria, size = 1, prob = c(pTCM)),
  ncol = nCriteria
)


for(c in 1:nCriteria) TCM[,c] <- rbinom(nTopics, size = 1, prob = pTCM[,c])



reviewers <- list()
for(r in 1:nReviewers) {
  
  # Reviewer initialization: error and bias_____________________________________
  reviewers[[r]] <- list(
    error = topicsError,
    bias = topicsBias
  )
  
  
  # Reviewer initialization: interpretation of the GL___________________________
  # We determine the reviewer's own interpretation of the grading language:
  gl <- sapply(1:(gradingScale - 1), FUN = function(i){
    truncate(rnorm( # Equation 3
      n = 1,
      mean = GLthresholds[i],
      sd = GLdiversity * (1 - ((i - 1) / gradingScale))
    ))
  })
  # Last, we order all thresholds in increasing order:
  reviewers[[r]]$gl <- gl <- gl[order(gl)]
  
  
  # Reviewer initialization: TC-Mapping_________________________________________
  # Here we rewire the "template" TC-mapping to create the mapping of
  # reviewer "r".
  # We start by determining how many links in the network we shall rewire.
  # Because we need to keep the network density constant, we rewire by 
  # swapping ties. So we start by calculating how many swaps we need to make
  tcm <- c(TCM)
  nSwaps <- round(TCMdiversity * nTopics * nCriteria)
  
  # Then we start swapping random couples of links.
  swaps <- 0
  repeat{
    linksToSwap <- sample(
      1:(nTopics * nCriteria),
      size = 2,
      replace = FALSE
    )
    tcm[linksToSwap] <- tcm[rev(linksToSwap)]
    
    swaps <- swaps + 1
    if(swaps >= nSwaps) break
  }
  rm(nSwaps, swaps)
  
  # We conclude by formatting the TC-mapping matrix appropriately:
  reviewers[[r]]$tcm <- tcm <- matrix(tcm, ncol = nCriteria)
  
  
  
  
  
  
  
}


#

















## From aggregation study:



# The most important argument in this function is the criteria data.frame,
simulation <- function (
  criteria = cbind.data.frame(
    name    = c("q1"),    # name of the criterion
    alpha   = c(3),       # alpha (parameter in the beta distribution)
    beta    = c(3),       # beta  (parameter in the beta distribution)
    scale   = c(5),       # scale (expressed as number of categories)
    gradeLanguage = c("asymmetric"), # Grade language: symmetric or asymmetric
    glh     = c(0.1),     # grade language heterogeneity
    weights = c(1)        # relative weight of the criteria 
  ),
  nSubmissions = 20,
  nReviewersPerProp = 5,
  nPropPerReviewer = 3, # max
  reviewerError = 0.2, # reviewer competence is defined as 1 - reviewerError
  reviewerVariability = 0.2,
  reviewerBias = 0,
  criteriaWeightsError = 0,
  aggrRule = c(
    "control",
    "mean",
    "hypermean",
    "lowestScore",
    "median",
    "majorityJudgement",
    "bordaCount"),
  nAccepted = c(5, 10),
  seed = runif(1, -999999999, 999999999)
)
{
  # Setting random seed:
  set.seed(seed)
  if (any(nAccepted > nSubmissions)){warning(
    "Please review parameter nAccepted: one or
    more values are higher than the number of submissions.")}
  
  # Saving the parameter configuration into an object:
  parameters <- list(
    criteria = criteria,
    nSubmissions = nSubmissions,
    nReviewersPerProp = nReviewersPerProp,
    nPropPerReviewer = nPropPerReviewer,
    reviewerError = reviewerError,
    aggrRule = aggrRule,
    nAccepted = nAccepted,
    seed = seed,
    timestamp = as.character(Sys.time())
  )
  #nEvaluationCriteria <- nrow(criteria)
  
  # Creating submissions. We fill in the true quality for each attribute,
  # drawing from the true quality distribution specified in the dataframe
  # "criteria":
  submissions <- data.frame(
    trueQuality = rbeta(
      n = nSubmissions,
      shape1 = criteria$alpha,
      shape2 = criteria$beta
    )
  )

  
  # Next, we calculate, for each submission, what would be the true ranking
  # position of the submissions.
  submissions$trueRanking <- rank(
    1 - submissions$trueQuality, # "1 -" allows to get low ranking number when
    na.last = "keep",            # the true quality is high.
    ties.method = "first"
  )
  
  # Now it's time to call in the reviewers, of which we create as many as we
  # need.
  # We start by creating a review network, where rows are reviewers and columns
  # are proposals:
  rnw <- allocationNetwork(
    nSubmissions = nSubmissions,
    nReviewersPerProp = nReviewersPerProp,
    nPropPerReviewer = nPropPerReviewer
  )
  
  # The review network tells us exactly how many reviewers we need:
  nReviewers <- nrow(rnw)
  
  # Next, we give reviewers the attributes they need - for now, they are given
  # two parameters which govern the amount of error and bias they have in
  # their judgment.
  # Also, for now we are assuming that they have the no bias when evaluating
  # different criteria.
  reviewers <- data.frame(
    id = 1:nReviewers,
    #error = rep(reviewerError, times = nReviewers),
    error = truncate(rnorm(
      n = nReviewers, mean = reviewerError, sd = reviewerVariability)),
    bias = rep(reviewerBias, times = nReviewers)
  )
  if(reviewerBias == "random") {
    reviewers$bias <- NULL
    reviewers$bias <- runif(min = -0.2, max = 0.2, n = nReviewers)
  }
  
  # Next, we give each reviewer a grade language interpretation - that is, a
  # vector of thresholds that determine how the continuous evaluation scale
  # is to be discretized. 
  # For practicality, we store all these values in lists, one per evaluation
  # criterion, and whose index identifies the reviewer the grade language
  # belongs to.
  gradeLanguages <- list()
  for (c in 1:nrow(criteria)){
    gradeLanguage <- createGradeLanguage(
      scholars = reviewers,
      criterion = criteria[c,]
    )
  }
  
  # Now we have all we need for reviewers to rate the submissions.
  # Each reviewer is going to rate the submission she's assigned to, giving a
  # score for each of the attributes, expressed in the correct scale as
  # specified in the criteria dataframe.
  #
  # So, we start by defining the objects where we will store the ratings:
  grades <- matrix(NA, nrow = nSubmissions, ncol = nReviewers)
  
  # For each reviewer and its of its submissions:
  for (rev in 1:nReviewers){ for (prop in which(rnw[rev,] == 1)){
    
    # Determine the grade:
    grades[prop,rev] <- rate(
      evaluatedAttribute = # Fix in case of combinatorial aggregation
        submissions$trueQuality[prop] + reviewers$bias[rev],
      sd = reviewers$error[rev],
      rule = "quality",
      thresholds = gradeLanguage[[rev]],
      categories = criteria$scale # Fix in case of combinatorial aggregation
    )
  }}
  
  
  # All is left to do is to aggregate.
  #
  # We create a list, where each item will pertain to the results of one of the
  # specified aggregation rules.
  r <- list()
  
  # Then, for each of the aggregation rules...
  for(rule in 1:length(aggrRule)){
    
    # ... we calculate the aggregate score for each proposal (x) ...
    if (
      aggrRule[rule] != "majorityJudgement" &
      aggrRule[rule] != "bordaCount" &
      aggrRule[rule] != "control" 
    ) {
      x <- aggregate(
        scores = grades,
        rule = aggrRule[rule],
        weights = criteria$weights,
        reviewers = reviewers
      )
      x <- x / (criteria$scale - 1) # Normalizing scores to range btw. 0 and 1
    }
    
    if (aggrRule[rule] == "control"){
      sampleReviewer <- sample(1:nReviewers, size = 1)
      x <- c()
      for (prop in 1:nSubmissions){
        x[prop] <- rate(
          evaluatedAttribute =
            submissions$trueQuality[prop] + reviewers$bias[sampleReviewer],
          sd = reviewers$error[sampleReviewer],
          rule = "quality",
          thresholds = gradeLanguage[[sampleReviewer]],
          categories = criteria$scale
        )
      }
      x <- x / (criteria$scale - 1)
    }
    
    if (aggrRule[rule] == "hypermean"){
      x <- hypermean(grades, dampingOutliers = TRUE)
      x <- x / (criteria$scale - 1)
    }
      
    if (aggrRule[rule] == "majorityJudgement"){
      x <- calcMajorityJudgement(grades)$majorityValue
      
      # We find the normalization term, which is the majorityValue that a
      #proposal could get if all its reviewers gave it the highest grade:
      magnitude <- 10 ^ (floor(log10(criteria$scale)) + 1)
      f <- (nReviewersPerProp:1) - 1
      norm <- rep(criteria$scale, times = nReviewersPerProp)
      for (c in 1:nReviewersPerProp){
        norm[c] <- norm[c] * (magnitude ^ f[c])
      }
      norm <- sum(norm)
      
      # Then we normalize:
      x <- x / norm
    }
    
    if (aggrRule[rule] == "bordaCount"){
      x <- modifiedBordaCount_extended(grades)$bordaCount
      x <- x / ((nSubmissions - 1) * nReviewersPerProp)# normalizing
    }
    
    
    # Then we calculate the resulting ranking position.
    # Note that what we rank is "1 / x". In this way, the ranking we obtain is
    # such that low ranking positions (e.g. 1st or 2nd) signify high aggregated
    # scores.
    if(aggrRule[rule] == "majorityJudgement"){
      g <- calcMajorityJudgement(grades)$majorityJudgment
    } else {
      g <- x
    }
    
    xr  <- rank(1 / g,  na.last = "keep", ties.method = "first")
    
    
    # Here we calculate, for each value of nAccepted, which proposals have a
    # trueQuality level worthy of acceptance, and which an estimQuality level
    # that would get them accepted. We store everything in a dedicated list.
    tqDeserved <- estimated <- list()
    for (a in 1:length(nAccepted)){
      
      # Deserving proposals (deserved == TRUE) are here defined as the proposals
      # whose objective grade is equal to or higher than the true quality of the
      # proposals on the cutoff threhsold.
      tqDeserved[[a]] <- submissions$trueQuality >=
        submissions[submissions$trueRanking==nAccepted[a],]$trueQuality
      
      # Estimated is set to TRUE for all proposals whose estimated quality is 
      # equal to or greater than the estimated quality of the proposal on the 
      # cutoff threshold:
      estimated[[a]] <- x >= x[xr == nAccepted[a]]
    }
    
    
    # Last, we calculate the outcome measures.
    # For every outcome variable we take two measurements:
    # - one that compares the panel grades / ranking with the true grades and 
    #   rankings;
    # - one that compares the panel grades / rankings with the grade/rankings 
    #   that an infallible panel would make (i.e. a panel with no reviewer
    #   error and 0 grade language heterogeneity).
    # The former allows to compare between aggregation rules; our main outcome
    # variables are of this kind. By contrast, the latter allows to gain better
    # insight into the performance of panels that adopt the same aggregation
    # rule.
    # To distinguish between these two measurements, the second one (panel vs
    # infallible panel) uses variable names that begin with "o*".
    #
    # So, we start with the error in the evaluation.
    # This outcome measure matches the main outcome variable from many
    # simulation models in the literature (e.g. those based on Squazzoni &
    # Gandelli 2012, 2013).
    # Note that this measure cannot be calculated when aggregation is done by
    # majority judgment. This is because majority judgment outputs only a 
    # ranking of proposals, and not an absolute grade for each.
    ifelse(
      aggrRule[rule] %in% c("majorityJudgement", "bordaCount"),
      qualityEfficacy <- NA,
      qualityEfficacy <- 1 - mean(abs(submissions$trueQuality - x))
    )
    #oQualityEfficacy <- 1 - mean(abs(ox - x))
    
    # Then we compare the two rankings: based on their true quality and their
    # estimated quality (x).
    # rankingEfficacy is low when many proposal on top of the true ranking are
    # not on top according to the estimated ranking.
    # Note that, despite the underlying strict ordering, the way we calculate
    # this measure is equivalent to assuming a weak ordering of submissions.
    rankingEfficacy <- c()
    #rankingEfficacy <- oRankingEfficacy <- c()
    
    # For every value of k (nAccepted) that we explore: 
    for (a in 1:length(nAccepted)){
      k = nAccepted[a]
      
      # The true quality of the the k-th best in the merit ranking:
      thT <- submissions$trueQuality[submissions$trueRanking == k]
      
      # The quality of the k-th best in the panel ranking:
      thE <- x[xr == k]
      
      # The list of proposals that deserve funding: those that have a true
      # quality at least equal to that of the k-th proposal in the merit
      # ranking.
      acceptableOnes <- which(submissions$trueQuality >= thT)
      
      
      # Submissions above the equivalence class of the k-th best:
      nonDiscretionaryPanelChoice <- which(x > thE)
      A <- length(nonDiscretionaryPanelChoice)
      
      # How many of these are right:
      A1 <- sum(nonDiscretionaryPanelChoice %in% acceptableOnes)
      
      # Submissions in k-th equivalence class (i.e. the discretionary
      # component of panel's choice):
      kthEquivClass <- which(x == thE)
      B <- length(kthEquivClass)
      
      # Of which, these many are right:
      B1 <- sum(kthEquivClass %in% acceptableOnes)
      
      # Choice performance for this level of k is:
      rankingEfficacy[a] <- (((k - A) * (B1 / B)) + A1) / k
    }
    
    # For the next outcome metrics, we need to have the weak orderings of
    # submissions (based on their true quality and estimated quality).
    weakOrdTrueQuality <- rank(submissions$trueQuality, ties.method = "max")
    weakOrdEstimQuality <- rank(x, ties.method = "max")
    
    # Now we calculate the normalized Kendall-tau distance between the two
    # weak orderings: based on trueQuality and based on estimated quality (x).
    # The Kendall distance is calculated for the whole ranking (kend), and for
    # the objectively-best proposals only (kendTop).
    ktd <- kendallTauDistance(
      weakOrdTrueQuality,
      weakOrdEstimQuality
    )$normalized
    
    ktdTop <- c()
    spearmanTop <- c()
    for (a in 1:length(nAccepted)){
      thT <- submissions$trueQuality[submissions$trueRanking == nAccepted[a]]
      top <- which(submissions$trueQuality >= thT)
      
      meritRank <- rank(submissions$trueQuality[top], ties.method = "max")
      panelRank <- rank(x[top], ties.method = "max")
      
      
      ktdTop[a] <- kendallTauDistance(
        meritRank,
        panelRank
      )$normalized

      spearmanTop[a] <- suppressWarnings(cor(
        meritRank,
        panelRank,
        method = "spearman"
      ))
      if (is.na(spearmanTop[a])) {spearmanTop[a] <- 0}
      
    }
    
    # We also calculate two correlation coefficients for the two rankings:
    ktc <- suppressWarnings(cor(
      weakOrdTrueQuality,
      weakOrdEstimQuality,
      method = "kendall"
    ))

    spearman <- suppressWarnings(cor(
      submissions$trueQuality,
      x,
      method = "spearman"
    ))
    
    # Sometimes it can happen that all proposals get the same aggregate score
    # and ranking position (e.g. when there are very few proposals, or when
    # the aggregation rule is lowestScore and there is a large number of
    # reviewers). When this happens, the correlation cannot be calculated
    # because one of the variables has no variability.
    # We interpret this as evidence that the aggregation rule performed
    # exceptionally poorly. For this reason, we replace the missing value
    # returned by the correlation function with a zero:
    if (is.na(ktc)) {ktc <- 0}
    if (is.na(spearman)) {spearman <- 0}
    
    
    # We store the outcome variables we have calculated in a dedicated list.
    outcomeMetrics <- list(
      
      # First, the measures that compare panel judgment with true merit:
      #bayesEfficacy = bayesEfficacy, 
      qualityEfficacy = qualityEfficacy,
      rankingEfficacy = rankingEfficacy,
      ktd = ktd,
      ktdTop = ktdTop,
      spearmanTop = spearmanTop,
      ktc = ktc,
      spearman = spearman
    )
    
    
    # Last, we add all data to the results list, and naming the dataframe
    # according to the appropriate aggregation rule adopted.
    r[[rule]] <- list(
      grades = grades,
      i = data.frame(
        estimQuality = x,
        estimRanking = xr
      ),
      tqDeserved = tqDeserved,
      estimated = estimated,
      outcomeMetrics = outcomeMetrics
    )
    names(r)[rule] <- aggrRule[rule]
    
  }
  
  # And we terminate the simulation by returning info on the initial
  # configuration and simulation outcomes:
  return(list(
    parameters = parameters,
    submissions = submissions,
    reviewers = reviewers,
    gradeLanguages = gradeLanguages,
    reviewNetwork = rnw,
    results = r
  ))
}




# Usage example_________________________________________________________________
if (FALSE) {
#  
#
sim <- simulation (
  criteria = cbind.data.frame(
    name    = c("q1"),    # name of the criterion
    alpha   = c(3),       # alpha (parameter in the beta distribution)
    beta    = c(3),       # beta  (parameter in the beta distribution)
    scale   = c(5),       # scale (expressed as number of categories)
    gradeLanguage = c("asymmetric"), # Grade language: symmetric or asymmetric
    glh     = c(0.1),     # grade language heterogeneity
    weights = c(1)        # relative weight of the criteria 
  ), 
  nSubmissions = 100,
  nReviewersPerProp = 20,
  nPropPerReviewer = 100,
  reviewerError = 0.2,
  reviewerVariability = 0.1,
  aggrRule = c(
    "mean",
    "excludeExtremes",
    "lowestScore",
    "median",
    "majorityJudgement",
    "bordaCount",
    "control"
  ),
  nAccepted = c(10, 20, 30, 40),
  seed = sample(-999999:999999, size = 1)
)
#
#
} 