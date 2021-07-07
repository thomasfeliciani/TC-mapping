# This script defines the main simulation function.
# At the end of the script there is some example code showing how to run it.
#rm(list=ls())

# Loading auxiliary functions and libraries:
source("./util.r")
library("faux")
library("irr")


simulation <- function(
  seed = sample(-99999999:99999999, size = 1),
  nReviewers = 5,
  nProposals = 10,
  attributeMean = 0.5,
  attributeSD = 0.2,
  attributeCorr = 0.5,
  nTopics = 5,
  nCriteria = 4,
  reviewerError = 0.2,
  reviewerBiasDiversity = 0.2,
  #reviewerNoise = 0.2, # influences reviewer error and bias
  GLdiversity = 0.2,
  gradingScale = 5, # number of categories in the evaluation scale
  TCMswapping = 0.2,
  pTCM = NULL
) {
  parameters <- c(
    seed = seed,
    nReviewers = nReviewers,
    nProposals = nProposals,
    attributeMean = attributeMean,
    attributeSD = attributeSD,
    attributeCorr = attributeCorr,
    nTopics = nTopics,
    nCriteria = nCriteria,
    reviewerError = reviewerError,
    reviewerBiasDiversity = reviewerBiasDiversity,
    #reviewerNoise = reviewerNoise,
    GLdiversity = GLdiversity,
    gradingScale = gradingScale,
    TCMswapping = TCMswapping
  )
  
  # Setting random seed
  set.seed(seed)
  
  # Creating a proposal with a given number of "cues"/topics of correlated
  # quality
  proposals <- faux::rnorm_multi(
    n = nProposals,
    mu = rep(attributeMean, times = nTopics),
    sd = rep(attributeSD, times = nTopics),
    r = attributeCorr,
    as.matrix = TRUE
  )
  proposals <- apply(X = proposals, MARGIN = 2, FUN = truncate)
  
  
  # Initializing the objects to store reviewer evaluations:
  gradesCrit <- matrix(NA, nrow = nReviewers, ncol = nCriteria)
  gradesOverall <- rep(NA, times = nReviewers)
  
  
  # A grade language is a vector of thresholds used to discretize the cues from
  # the proposals (continuous variable) into a discrete grade.
  # Here we define the "standard" grade language, to which reviewers will later
  # try to comply more-or-less accurately (due to their own understanding of
  # the grading language).
  #GLthresholds <- c()
  #for (l in 1:(gradingScale - 1)){
  #  GLthresholds[l] <- 1 - ((3 / 5) ^ l) 
  #}
  GLthresholds <- qbeta(
    1:(gradingScale - 1) / gradingScale,
    shape1 = 2, shape2 = 1
  )
  
  
  # We also create a "template" TC-mapping from which each reviewer's own
  # mapping will deviate to a specified degree. The "template" mapping is
  # created by making links between topics and criteria with a probability 
  # equal to the relative frequency of that link from the survey responses.
  # Unlike the raw survey microdata that cannot be shared, the probability
  # weights constitute aggregate statistics and thus can be (and are) included
  # in the GitHub repository.
  # In our intentions, this way of creating TC-mappings allows for realistic
  # mappings in the simulation while preserving full reproducibility of our
  # results. It allows for easy manipulation of the TC-mapping network,
  # specifically of its size (number of topics and of criteria) and inter-
  # -reviewer diversity.
  #
  # This contains the weights for a TC-mapping template:
  if (is.null(pTCM)) load("./data/pTCM.RData")
  #
  #
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
  
  
  reviewers <- list()
  for(r in 1:nReviewers) {
    reviewers[[r]] <- list()
    # Reviewer initialization: error and bias___________________________________
    reviewers[[r]]$bias <- rnorm(
      n = 1,
      mean = 0,
      sd = reviewerBiasDiversity
    )
    
    reviewers[[r]]$error <- reviewerError
    #rnorm(n = nTopics, mean = 0, sd = reviewerNoise)
    #rep(reviewerNoise, times = nTopics)
    
    
    # Reviewer initialization: interpretation of the GL_________________________
    # We determine the reviewer's own interpretation of the grading language:
    gl <- truncate(rnorm(
      n = gradingScale - 1,
      mean = GLthresholds,
      sd = GLdiversity
    ))
    
    # Last, we order all thresholds in increasing order:
    reviewers[[r]]$gl <- gl[order(gl)]
    
    
    # Reviewer initialization: TC-Mapping_______________________________________
    # Here we rewire the "template" TC-mapping to create the mapping of
    # reviewer "r".
    # We start by determining how many links in the network we shall rewire.
    # Because we need to keep the network density constant, we rewire by 
    # swapping ties. So we start by calculating how many swaps we need to make
    tcm <- c(TCM)
    nSwaps <- round(TCMswapping * nTopics * nCriteria)
    
    # Then we start swapping random couples of links.
    swaps <- 0
    repeat{
      if(swaps >= nSwaps) break
      linksToSwap <- sample(
        1:(nTopics * nCriteria),
        size = 2,
        replace = FALSE
      )
      tcm[linksToSwap] <- tcm[rev(linksToSwap)]
      
      swaps <- swaps + 1
    }
    rm(nSwaps, swaps)
    
    # We conclude by formatting the TC-mapping matrix appropriately:
    reviewers[[r]]$tcm <- tcm <- matrix(tcm, ncol = nCriteria)
    
    
    
    # Reviewing proposals______________________________________________________
    #
    # 1) Bias and error in the perception of the cues/topics.
    # For each reviewer r we create a matrix of perception values, where rows
    # are proposals and columns are topics.
    reviewers[[r]]$topicEval <- t(apply(
      X = proposals,
      MARGIN = 1,
      FUN = function(prop) {
        return(truncate(rnorm(
          n = nTopics,
          mean = prop + rep(reviewers[[r]]$bias, times = nTopics),
          sd = reviewers[[r]]$error
        )))
      }
    ))
    
    # 2) Producing criterial evaluations based on the reviewer's TC-mapping.
    # We store data in a matrix where rows are proposals and columns are
    # criteria.
    reviewers[[r]]$critEval <- t(apply(
      X = reviewers[[r]]$topicEval,
      MARGIN = 1,
      FUN = function(x){
        sapply(
          X = 1:nCriteria,
          FUN = function(c) {
            weighted.mean(x, w = tcm[,c])
          }
        )
      }
    ))
    
    # 3) Producing criterial grades based on the reviewer's interpretation of
    #    the grading language. This works by discretizing the evaluation scale
    #    (in [0,1]) into the discrete grading language.
    reviewers[[r]]$critGrades <- apply(
      X = reviewers[[r]]$critEval,
      MARGIN = c(1, 2),
      FUN = function(x) findInterval(x, vec = reviewers[[r]]$gl) + 1
    )
    reviewers[[r]]$overallGrades <- apply(
      X = reviewers[[r]]$critEval,
      MARGIN = 1,
      FUN = function(x){
        findInterval(x = mean(x, na.rm = TRUE), vec = reviewers[[r]]$gl) + 1
      }
    )
  }
  
  
  # Calculating outcome variables_______________________________________________
  #
  #
  # TC-mapping heterogeneity.
  # We calculate the hamming distance in TC-mapping between all pairs of
  # reviewers. We start by finding all possible pairs of reviewers:
  reviewerPairs <- as.data.frame(t(combn(1:nReviewers, m = 2)))
  #diff$d <- NA
  
  # Then we calculate the hamming distance between all pairs. Simply, the
  # function "twdis" counts the differences between two same-sized networks,
  # and normalizes the tally by dividing it by the total number of possible
  # differences (i.e. the number of ties).
  reviewerPairs$TCMdissimilarity <- apply(
    X = reviewerPairs,
    MARGIN = 1,
    FUN = function(x) {
      twdis(
        a = reviewers[[x[1]]]$tcm,
        b = reviewers[[x[2]]]$tcm,
        normalized = TRUE
      )
    }
  )
  
  # Finally, the average normalized hamming distance is what we call
  # TC-mapping diversity, which should correlate with TCMswapping.
  TCMdiversity <- mean(reviewerPairs$TCMdissimilarity)
  
  
  
  # Inter-rater reliability (IRR).
  overallGrades <- sapply(X = reviewers, FUN = "[[", "overallGrades")
  
  # For the ICC we calculate a two-way random model, since our reviewers were
  # chosen at random from a set of all possible reviewers, and each reviewer
  # rates all proposals.
  ICC <- suppressWarnings(irr::icc(
    ratings = overallGrades,
    model = "twoway",
    type = "agreement",
    unit = "single"#"average"
  ))$value
  #psych::ICC(x = overallGrades)
  if (is.nan(ICC)) ICC <- NA
  
  
  # We also calculate reviewer agreement by averaging the average pairwise
  # correlation among reviewers. Since reviewer grades are on an ordinal scale,
  # We use Spearman's Rho ranking correlation coefficient.
  reviewerPairs$spearmanRho <- apply(
    X = reviewerPairs,
    MARGIN = 1,
    FUN = function(pair) {
      suppressWarnings(cor(
        x = reviewers[[pair[1]]]$overallGrades,
        y = reviewers[[pair[2]]]$overallGrades,
        method = "spearman",
        use = "pairwise.complete.obs"
      ))
    }
  )
  spearmanRho <- mean(reviewerPairs$spearmanRho, na.rm = TRUE)
  if(is.nan(spearmanRho)) spearmanRho <- NA
  
  
  
  # Last, we also check to what degree the review panel *as a whole* was able
  # to estimate the ranking of proposals. Proposal ranking is inferred from
  # the average of their attribute values.
  meanGrades <- apply(X = overallGrades, MARGIN = 1, FUN = mean, na.rm = TRUE)
  ifelse(
    length(table(meanGrades)) == 1, # If the panel didn't make any distictions..
    rankingPerf <- 0,
    rankingPerf <- suppressWarnings(cor(
      x = apply(X = proposals, MARGIN = 1, FUN = mean),
      y = meanGrades,
      method = "spearman",
      use = "pairwise.complete.obs"
    ))
  )
  
  
  
  return(list(
    summary = c(
      parameters,
      TCMdiversity = TCMdiversity,
      ICC = ICC,
      spearmanRho = spearmanRho,
      rankingPerf = rankingPerf
    ),
    proposals = proposals,
    reviewers = reviewers,
    reviewerPairs = reviewerPairs
  ))
}





#_______________________________________________________________________________
#
# Example run 
#_______________________________________________________________________________
if (FALSE) {
  sim <- simulation(
    seed = 12345,
    nReviewers = 5,
    nProposals = 10,
    attributeMean = 0.5,
    attributeSD = 0.3,
    attributeCorr = 0.5,
    nTopics = 5,
    nCriteria = 4,
    reviewerError = 0.2,
    reviewerBiasDiversity = 0.2,
    GLdiversity = 0.2,
    gradingScale = 5,
    TCMswapping = 0,
    pTCM = NULL
  )
}







