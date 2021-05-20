
# Clearing environment and loading libraries and resources.
rm(list = ls())
library("ggplot2")
library("reshape", include.only = "melt")
source("util.r")

load("./data/unshareable/survey.RData") # Loading survey 


# First we look into the survey demographics, comparing them withe the
# composition of the population of SFI reviewers.
#
# Formatting data so it can be easily plotted:
a <- melt(s$populationDemographics$gender, id.vars = "class")
a$facet <- "gender"
a$class[a$class == "prefer not to say / non-response"] <-
  "prefer not to say"

b <- melt(s$populationDemographics$country, id.vars = "class")
b$facet <- "institution country"
b$class[b$class == "other EU country"] <- "European Union"
b <- b[b$class != "non-response",]

c <- melt(s$populationDemographics$institution, id.vars = "class")
c$facet <- "background"
c$class[c$class == "other/non-response"] <-
  "other /\nprefer not to say"

d <- melt(s$populationDemographics$program, id.vars = "class")
d$facet <- "reviewed for"
d$class[d$class == "only Industry Fellowship"] <-
  "Industry Fellowship"
d$class[d$class == "only Investigators Programme"] <-
  "Investigators Programme"
d$class[d$class == "both"] <- "both programs"
d$class[d$class == "neither/non-response"] <-
  "neither"

demo <- rbind(a, b, c)#, d)
rm(a, b, c, d)

demo$class <- factor(x = demo$class, levels = unique(demo$class))
demo$facet <- factor(x = demo$facet, levels = unique(demo$facet))
demo$value <- demo$value * 100
#demo$variable <- factor(x = demo$class, levels = unique(demo$class))

png(
  filename = "./outputGraphics/surveyDemographicsSmall.png",
  width = 1400, height = 800, res = 300, units = "px"
)
ggplot(demo, aes(x = class, y = value, fill = variable)) + 
  geom_col(
    position = position_dodge2(reverse = FALSE, width = NULL, padding = 0),
    width = 0.5, color = "black", size = 0.5
  ) +
  #scale_x_discrete(limits = rev) +
  facet_wrap(facets = demo$facet, nrow = 1, scales = "free_x") +
  ylab("relative frequency") +
  #scale_fill_discrete(
  scale_fill_viridis_d(
    name = "", begin = 1, end = 0.5, #begin = 0.85, end = 0.1
    labels = c(
      "SFI reviewers\nN = 1591 (population)",
      "survey respondents\nN = 310 (sample)"
    )
  ) +
  scale_y_continuous(
    limits = c(0,100), expand = c(0,0), breaks = c(0, 25, 50, 75, 100),
    labels = c("0%", "25%", "50%", "75%", "100%")
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 0, "pt"),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray85", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "gray95"),#element_blank(),
    legend.position = "top",
    legend.background = element_rect(fill = "gray95"),
    strip.background = element_blank(),#element_rect(fill = "gray95"),
    axis.title = element_blank(),
    axis.line.y = element_line(colour = "black"),
    axis.line.x = element_blank(),
    axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))
dev.off()

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

# We also calculate the relative frequency of each edge in the network (i.e.
# each matching of topics-criteria) and we save it to file. These are used by
# the simulation script as calibration data to generate realistic TC-mappings.
pTCM <- matrix(0, nrow = 12, ncol = 3)
for (m in 1:length(TCM)) pTCM <- pTCM + TCM[[m]]
for (c in 1:3) for (r in 1:12) pTCM[r,c] <- pTCM[r,c] / length(TCM)
save(pTCM, file = "./data/pTCM.RData")


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



# TC-mapping plots
#
topics <- data.frame(x = rep(0, times = 12), y = 1:12)
criteria <- data.frame(x = c(1, 1, 1), y = c(3, 6, 9))
edges <- data.frame(
  x = rep(0, times = 36),
  xend = rep(0.9, times = 36),
  y = c(1:12),
  yend = c(
    (((-5:6) - 0.5) * 0.12) + 3,
    (((-5:6) - 0.5) * 0.12) + 6,
    (((-5:6) - 0.5) * 0.12) + 9
  )
  #yend = c(rep(4, times = 12), rep(6, times = 12), rep(8, times = 12))
)
nodeLabels <- data.frame(
  x = c(0, 1),
  y = c(13, 12),
  lab = c("review topics", "evaluation criteria")
)
topicLabels <- data.frame(
  x = rep(-0.1, times = 12),
  y = 1:12,
  lab = s$q27$topics
)
criteriaLabels <- data.frame(
  x = rep(1.1, times = 3),
  y = c(3, 6, 9),
  lab = c("Potential for impact", "Proposed research", "Applicant")
)

png(
  filename = "./outputGraphics/TC-mapping.png",
  width = 2000,
  height = 1050,
  res = 300,
  units = "px", bg = "white"#"transparent"
)
ggplot() +
  geom_segment(
    data = edges,
    aes(x = x, xend = xend, y = y, yend = yend),# alpha = 0.9,
    #arrow.fill = alpha("black", 0.1),
    arrow = arrow(angle = 5, type = "closed", length = unit(7, "pt"))
  ) +
  geom_point(data = topics, aes(x = x, y = y), size = 4, color = "darkorange") +
  geom_point(data = criteria, aes(x = x, y = y), size = 4, color="darkorange") +
  geom_text(
    data = nodeLabels, color = "darkorange1", 
    aes(x = x, y = y, label = lab, fontface = 3)
  ) +
  geom_text(
    data = topicLabels, hjust = 1,
    aes(x = x, y = y, label = lab)
  ) +
  geom_text(
    data = criteriaLabels, hjust = 0,
    aes(x = x, y = y, label = lab)
  ) +
  coord_cartesian(clip = 'off') +
  theme(
    plot.margin = unit(c(0, 6.5, 0, 16), "lines"),
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank()
  )
dev.off()




edgeLabels <- data.frame(
  x = c(-0.75, 0.5, 1.5, 2.75),
  y = c(0, 1, 4, 5),
  lab = c(
    "reviewer perception can be\ndistorted by error and biases",
    "reviewer's\nTC-mapping",
    "aggregation",
    "applying reviewer's\ngrading standards\nto find the appropriate grade"
  )
)
nodeLabels <- data.frame(
  x = c(-1.5, 0, 1, 2, 3.5),
  y = c(13, 13, 9, 7.5, 7),
  lab = c(
    "proposal\nattributes",
    "review topics",
    "evaluation\ncriteria",
    "reviewer's\noverall opinion",
    "reviewer\nevaluation"
  )
)

test <- data.frame(
  x = c(2),
  y = c(11), lab = c(
    'paste("test\n", bold("grassetto"))'
    #'expression("test\n", bold("grassetto"))'
    #'expression("Hair " * phantom("color") ,col.main="red")'
    #'#'atop(atop(bold("bold"),"plain"),"3rd line")')
  )
    
)
lab = c(
  expression("nero arancio accapo")
)

#png(
#  filename = "./outputGraphics/TC-mapping2.png",
#  width = 5000, height = 1500,
#  res = 300, units = "px", bg = "transparent"
#)
ggplot() +
  geom_segment(aes( # "reviewer perception"
    x = rep(-1.5, times = 12), xend = rep(-0.05, times = 12),
    y = 1:12, yend = 1:12
  ), linetype = "dashed", color = "gray50",
  arrow = arrow(angle = 15, type = "closed", length = unit(10, "pt"))) +
  geom_segment(
    data = edges,
    aes(x = x, xend = xend, y = y, yend = yend)
  ) +
  geom_segment(
    aes(
      x = c(1, 1, 1), xend = c(1.95, 1.95, 1.95),
      y = c(4, 6, 8), yend = c(5.95, 6, 6.05)),
    color = "black",
    arrow = arrow(angle = 10, type = "closed", length = unit(10, "pt"))
  ) +
  geom_segment(
    aes(x = 2, xend = 3.45, y = 6, yend = 6), color = "black",
    arrow = arrow(angle = 15, type = "closed", length = unit(10, "pt"))
  ) +
  geom_point(
    aes(x = rep(-1.5, times = 12), y = 1:12), size = 4, color = "black") +
  geom_point(data = topics, aes(x = x, y = y), size = 4, color = "darkorange") +
  geom_point(
    data = criteria, aes(x = x, y = y), size = 4, color = "darkorange") +
  geom_point(aes(x = c(2, 3.5), y = c(6, 6)), size = 4, color = "darkorange") +
  
  geom_text(
    data = test, parse = TRUE,
    aes(x = x, y = y, label = lab)
  ) +
  
  geom_text(
    data = edgeLabels,# parse = TRUE,
    aes(x = x, y = y, label = lab)
  ) +
  geom_text(
    data = nodeLabels,
    aes(x = x, y = y, label = lab)
  ) +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank()
  )

#dev.off()
