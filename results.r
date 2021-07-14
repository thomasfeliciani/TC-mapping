
# Clearing environment and loading libraries and resources.
rm(list = ls())
library("ggplot2")
library("reshape", include.only = "melt")
library("ggpubr")
source("util.r")
load("./data/unshareable/survey.RData") # Loading survey 

exportFormat = "png" # "png" or "tiff" are supported.




# Figure 1 _____________________________________________________________________
# Examples of TC mappings
#
topics <- data.frame(x = rep(0, times = 6), y = 1:6)
criteria <- data.frame(x = c(1, 1, 1), y = c(2, 3.5, 5))
edges <- data.frame(
  x = rep(0, times = 18),
  xend = rep(0.95, times = 18),
  y = c(1:6),
  yend = c(
    rep(2, times = 6),
    rep(3.5, times = 6),
    rep(5, times = 6)
  )
)
edges <- edges[18:1,]; row.names(edges) <- 1:18 # makes it easier to read
edges1 <- edges[c(1, 4, 5, 7, 8, 15),]
edges2 <- edges[c(1, 4, 5, 7, 14, 15, 18),]
#nodeLabels <- data.frame(
#  x = c(0, 1), y = c(7, 6), lab = c("review topics", "evaluation criteria")
#)
topicLabels <- data.frame(
  x = rep(-0.1, times = 6),
  y = topics$y,
  lab = rev(sapply(1:6, FUN = function(x){paste("topic", x)}))
)
criteriaLabels <- data.frame(
  x = rep(1.1, times = 3),
  y = criteria$y,
  lab = c("criterion C", "criterion B", "criterion A")
)

plotTCM <- function(edges) {ggplot() +
  geom_segment(
    data = edges,
    aes(x = x, xend = xend, y = y, yend = yend),
    arrow = arrow(angle = 12, type = "closed", length = unit(7, "pt"))
  ) +
  geom_point(data = topics, aes(x = x, y = y), size = 4, color = "darkorange") +
  geom_point(data = criteria, aes(x = x, y = y), size = 4, color="darkorange") +
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
    plot.margin = unit(c(2, 6, 0, 4), "lines"),
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank()
  )
}

figure2 <- ggarrange(
  plotTCM(edges1), plotTCM(edges2),
  labels = c("reviewer #1", "reviewer #2"),
  ncol = 2, hjust = -1
)
#plot(figure2)

figureParameters <- list(
  filename = paste0("./outputGraphics/Fig1.", exportFormat),
  width = 2000,
  height = 700,
  units = "px",
  res = 300, bg = "white"#"transparent"
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

plot(figure2)

dev.off()





# Figure 2 _____________________________________________________________________
# SFI 12-by-3 TC-mapping: all possible links
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
  #lab = s$q27$topics
  lab = c(
    #row.names(df) <- c(
    "research environment and infrastructure",
    "research design",
    "relevance/importance of the topic",
    "projected timeframe",
    "novelty of proposed research",
    "mitigating risk",
    "links with other research institutions/companies",
    "likelihood/chance of success",
    "knowledge/technology transfer",
    "economic/societal value of requested budget",
    "applicants' track record",
    "applicants' expertise on the topic"
  )
)
criteriaLabels <- data.frame(
  x = rep(1.1, times = 3),
  y = c(3, 6, 9),
  lab = c("potential for impact", "proposed research", "applicant")
)

figureParameters <- list(
  filename = paste0("./outputGraphics/Fig2.", exportFormat),
  width = 2000,
  height = 1050,
  units = "px",
  res = 300
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}
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




# Figure 3 _____________________________________________________________________
# Survey: TC-mapping heatmap
#
load("./data/pTCM.RData") # Loading relative frequencies for TCM (from survey)
df <- as.data.frame(pTCM)
names(df) <- c("applicant", "proposed research", "potential for impact")
df$topic <- c(
  #row.names(df) <- c(
  "research environment and infrastructure",
  "research design",
  "relevance/importance of the topic",
  "projected timeframe",
  "novelty of proposed research",
  "mitigating risk",
  "links with other research institutions/companies",
  "likelihood/chance of success",
  "knowledge/technology transfer",
  "economic/societal value of requested budget",
  "applicants' track record",
  "applicants' expertise on the topic"
)
df <- reshape::melt(df, id.vars = "topic")


figureParameters <- list(
  filename = paste0("./outputGraphics/Fig3.", exportFormat),
  width = 1400,
  height = 1100,
  res = 300, units = "px"
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

ggplot(data = df, aes(x = variable, y = topic, fill = value)) +
  geom_tile() +
  scale_y_discrete(limits = rev, expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0)) +
  scale_fill_viridis_c(
    begin = 0.1, end = 1, option = "A",
    limits = c(0,1), breaks = 0.25 * 0:4
  ) +
  labs(
    fill = "relative\nfrequency",
    x = "evaluation criteria",
    title = "topics"
  ) +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 12, hjust = -1.5),
    axis.text.x = element_text(angle = 35, hjust = 1),
    axis.title.y = element_blank(),
    axis.line = element_blank(),
    legend.background = element_rect(fill = "gray95")
  )

dev.off()




# Figure 4 _____________________________________________________________________
# Simulation overview
#
topics <- data.frame(x = rep(0, times = 12), y = 1:12)
criteria <- data.frame(x = c(1, 1, 1), y = c(3, 6, 9))
edges <- data.frame(
  x = rep(0, times = 36),
  xend = rep(1, times = 36),
  y = c(1:12),
  yend = c(
    rep(3, times = 12),#(((-5:6) - 0.5) * 0.12) + 3,
    rep(6, times = 12),#(((-5:6) - 0.5) * 0.12) + 6,
    rep(9, times = 12)#(((-5:6) - 0.5) * 0.12) + 9
  )
  #yend = c(rep(4, times = 12), rep(6, times = 12), rep(8, times = 12))
)
edgeLabels <- data.frame(
  x = c(-0.8, 0.5, 1.5, 2.75),
  y = c(-0.2, 0.8, 3, 4.5),
  lab = c(
    "reviewer perception distorted by\n", #error and biases
    "reviewer's\n", # TC-mapping
    "aggregation",
    "reviewer's\n\n" # grading standards
  )
)
edgeLabelsAccent <- data.frame(
  x = edgeLabels$x,
  y = edgeLabels$y,
  lab = c(
    "\nerror and biases",
    "\nTC-mapping",
    "",
    "\ninterpretation of\nthe grading language"
  )
)
nodeLabels <- data.frame(
  x = c(-1.5, 0, 1, 2, 3.5),
  y = c(13, 13, 11, 8, 7.5),
  lab = c(
    "proposal\nattributes",
    "review topics",
    "evaluation\ncriteria",
    "reviewer's\noverall opinion",
    "review\ngrade"
  )
)


figureParameters <- list(
  filename = paste0("./outputGraphics/Fig4.", exportFormat),
  width = 2300,
  height = 1200,
  units = "px",
  res = 300
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

ggplot() +
  geom_segment(aes( # "reviewer perception"
    x = rep(-1.5, times = 12), xend = rep(-0.05, times = 12),
    y = 1:12, yend = 1:12
  ), linetype = "dashed", color = "gray50",
  arrow = arrow(angle = 15, type = "closed", length = unit(10, "pt"))) +
  geom_segment( # TCM
    data = edges,
    aes(x = x, xend = xend, y = y, yend = yend)
  ) +
  geom_segment( # aggregation
    aes(
      x = c(1, 1, 1), xend = c(1.95, 1.95, 1.95),
      y = c(3, 6, 9), yend = c(5.95, 6, 6.05)),
    color = "black",
    arrow = arrow(angle = 10, type = "closed", length = unit(10, "pt"))
  ) +
  geom_segment( # optinion-to-grade conversion
    aes(x = 2, xend = 3.45, y = 6, yend = 6), color = "black",
    arrow = arrow(angle = 15, type = "closed", length = unit(10, "pt"))
  ) +
  geom_point(
    aes(x = rep(-1.5, times = 12), y = 1:12), size = 4, color = "black") +
  geom_point(data = topics, aes(x = x, y = y), size = 4, color = "darkorange") +
  geom_point(
    data = criteria, aes(x = x, y = y), size = 4, color = "darkorange") +
  geom_point(aes(x = c(2, 3.5), y = c(6, 6)), size = 4, color = "darkorange") +
  geom_text(data = edgeLabels, aes(x = x, y = y, label = lab)) +
  geom_text(
    data = edgeLabelsAccent, color = "darkorange",
    aes(x = x, y = y, label = lab)
  ) +
  geom_text(
    data = nodeLabels,
    aes(x = x, y = y, label = lab)
  ) +
  theme(
    plot.margin = margin(t = 4, r = 4, b = 10, l = 3, unit = "pt"),
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank()
  )

dev.off()





# Figure 5 _____________________________________________________________________
# Interpretation of a grading scale
th <- qbeta(1:4 / 5, shape1 = 2, shape2 = 1)
grades <- c("very bad", "average", "good", "very good", "outstanding")

# We'll put the grade labels in-between its top and bottom thresholds:
gradePos = sapply(1:5, function(x){th = c(0, th, 1); return(mean(th[x:(x+1)]))})


figureParameters <- list(
  filename = paste0("./outputGraphics/Fig5.", exportFormat),
  width = 1200,
  height = 350,
  units = "px",
  res = 300
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

ggplot() +
  geom_segment(
    aes(x = 0, y = 0, xend = 1, yend = 0),
    color = "darkorange1"#alpha("darkorange1", 0.5) 
  ) +
  geom_point(
    aes(x = c(0, th, 1), y = rep(0, times = length(th) + 2)),
    color = "white", shape = 15, size = 2) +# masks the orange line
  geom_text( # threshold marks
    aes(x = th, y = rep(0, times = length(th)), label = "|"),
    size = 3, color = "black") +
  geom_text( # grade label
    aes(x = gradePos, y = 2, label = grades),
    size = 3, hjust = c(0.5,0.5,0.5,1,1), angle = c(0,0,0,330,330)#330
  ) +
  geom_text( # opening square brackets
    aes(x = 0, y = 0, label = "["), size = 3, color = "black"
  ) +
  geom_text( # closing square brackets
    aes(x = 1, y = 0, label = "]"), size = 3, color = "black"
  ) +
  scale_x_continuous(limits = c(-0, 1), expand = c(0.004,0), breaks = 0:5/5) +
  scale_y_continuous(limits = c(-4, 9), expand = c(0, 0)) +
  labs(
    x = bquote('reviewer opinion (' ~ o[ip] ~ ')'), y = ""
  ) +
  theme(
    #plot.margin = margin(t = 30, r = 5, b = 5, l = 5, unit = "pt"),
    legend.position = "none",
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.x = element_line(color = "gray95"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.line = element_blank(),
  )

dev.off()








# Figure 6 _____________________________________________________________________

granul <- c(2, 5, 10)#c(2,5,10,20)
for (gr in 1:length(granul)) {
  scale = granul[[gr]]
  th <- data.frame(
    granularity = granul[[gr]],
    th = qbeta(1:(scale - 1) / scale, shape1 = 2, shape2 = 1)
  )
  ifelse(gr == 1, d <- th, d <- rbind(d, th))
}
granul <- factor(granul, levels = c("5", "2", "10")) # reordering factor levels


th <- ggplot(data = d, aes(x = th, y = as.factor(granularity))) +
  geom_segment(
    data = data.frame(
      x = rep(0, times = length(granul)),
      xend = rep(1, times = length(granul)), y = granul, yend = granul
    ),
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "darkorange1"#alpha("darkorange1", 0.5) 
  ) +
  geom_point(color = "white", shape = 15, size = 2) +# masks the orange line
  geom_point( # masks the line at the lowest and highest limit of the scale
    data = data.frame(
      granularity = c(granul, granul),
      th = c(rep(0, times = length(granul)), rep(1, times = length(granul)))
    ),
    color = "white", shape = 15, size = 1
  ) +
  geom_text(data = d, aes(label = "|"), size = 3, color = "black") +
  geom_text( # opening square brackets
    data = data.frame(
      granularity = granul, th = rep(0, times = length(granul))
    ), 
    aes(label = "["), size = 3, color = "black"
  ) +
  geom_text( # closing square brackets
    data = data.frame(
      granularity = granul, th = rep(1, times = length(granul))
    ), 
    aes(label = "]"), size = 3, color = "black"
  ) +
  scale_x_continuous(limits = c(0, 1), expand = c(0.004,0), breaks = 0:5/5) +
  scale_y_discrete(
    limits = rev(levels(granul)),
    labels = c("s=10", "s=2", "s=5")
  ) +
  labs(
    #x = "merit", y = ""
    x = bquote('reviewer opinion (' ~ o[ip] ~ ')'), y = ""
  ) +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.x = element_line(color = "gray95"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line = element_blank(),
    axis.text.y = element_text(size = 11, color = "black")
  )


quantiles <- data.frame(
  x = qbeta(1:4 / 5, shape1 = 2, shape2 = 1),
  xend = qbeta(1:4 / 5, shape1 = 2, shape2 = 1),
  y = c(0, 0, 0, 0),
  yend = dbeta(qbeta(1:4 / 5, shape1 = 2, shape2 = 1), shape1 = 2, shape2 = 1)
)
betapdf <- data.frame(
  x = 1:100/100,
  y = dbeta(1:100/100, shape1 = 2, shape2 = 1))

pdf <- ggplot() + # probability density function
  geom_line(data = betapdf, aes(x = x, y = y), linetype = "dashed") +
  geom_area(
    data = betapdf, aes(x = x, y = y), fill = alpha("black", 0.08)
  ) +
  geom_segment(
    data = quantiles,
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "black"
  ) +
  geom_text(aes(
    x = qbeta(1:4 / 5, shape1 = 2, shape2 = 1) - 0.03,
    y = rep(0.45, times = 4),#quantiles$yend / 2,
    label =
      c("1st quintile", "2nd quintile", "3rd quintile", "4th quintile")
  ), size = 3, angle = 90, color = "black"
  ) +
  scale_x_continuous(limits = c(0, 1), expand = c(0.004,0), breaks = 0:5/5) +
  scale_y_continuous(expand = c(0, 0)) +
  labs (y = "s=5\ndensity function") +#, x = "merit") +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "gray95"),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "darkorange1"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank() 
  )

figure6 <- ggarrange(
  pdf, th,
  labels = c("A", "B"),
  ncol = 1,
  heights = c(1.2,1),
  align = "v"
)
#plot(figure3)


figureParameters <- list(
  filename = paste0("./outputGraphics/Fig6.", exportFormat),
  width = 1000,
  height = 1000,
  units = "px",
  res = 300
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

plot(figure6)

dev.off()












################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# Simulation results

load("./output/ri.RData")

baseline = c(
  nReviewers = 5,
  nProposals = 10,
  attributeMean = 0.75,
  attributeSD = 0.2,
  attributeCorr = 0.5,
  nTopics = 12,
  nCriteria = 3,
  reviewerError = 0.1,
  reviewerBiasDiversity = 0.1,
  GLdiversity = 0.1,
  gradingScale = 5
)
variables <- c(
  "nReviewers", "nProposals", "attributeMean", "attributeSD", "attributeCorr",
  "nTopics", "nCriteria", "reviewerError", "reviewerBiasDiversity",
  "GLdiversity", "gradingScale")

#focusVariable = "gradingScale"
focusVariable = ""

closeToBaseline <- apply(
  X = ri,
  MARGIN = 1,
  FUN = function(x) {
    ifelse(
      any(x[variables[variables != focusVariable]] != baseline),
      return(FALSE),
      return(TRUE)
    )
  }
)
rii <- ri[closeToBaseline,]




# Figure 7 _____________________________________________________________________
# baseline: TCMswapping and IRR
#
figureParameters <- list(
  filename = paste0("./outputGraphics/Fig7.", exportFormat),
  width = 1400,
  height = 1000,
  res = 300, units = "px"
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

ggplot(
  data = rii, aes(x = TCMdiversity, y = ICC, color = as.factor(TCMswapping))
) +
  geom_vline( # reference line (TCMdiversity at SFI)
    xintercept = 0.3696205, linetype = 2, color = "black"
  ) +
  geom_point(size = 0.8) +
  geom_text(
    aes(x = 0.35, y = 0.9, angle = 90), color = "black", size = 3,
    label = "SFI reviewers"
  ) +
  scale_color_viridis_d(begin = 0.5, end = 0.92, option = "A") +
  labs(
    x = "TC-mapping heterogeneity\n(average normalized Hamming distance)",
    y = "inter-rater reliability (ICC)",
    color = "ρ"
  ) +
  scale_y_continuous(limits = c(0,1)) +
  theme(
    panel.background = element_blank(),
    plot.background = element_rect(fill="transparent", color=NA),
    panel.border = element_blank(),#rect(fill="transparent", color="gray50"),
    panel.grid.major.x = element_line(color = "gray95"),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 1)#,
    #legend.position = "NA"
  )

dev.off()




# Figure 8 _____________________________________________________________________
# baseline: TCMswapping and IRR
#
figureParameters <- list(
  filename = paste0("./outputGraphics/Fig8.", exportFormat),
  width = 1000,
  height = 1000,
  res = 300, units = "px"
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

ggplot(
  data = rii,
  aes(
    x = as.factor(TCMswapping),
    y = ICC,
    fill = as.factor(TCMswapping)
  )
) +
  geom_violin(color = "gray80", fill = "gray80", width = 0.8) +
  geom_boxplot(width = 0.3) +
  scale_color_viridis_d(begin = 0.5, end = 0.92, option = "A") +
  scale_fill_viridis_d(begin = 0.5, end = 0.92, option = "A") +
  labs(
    x = "TC-mapping heterogeneity (ρ)",
    y = "inter-rater reliability (ICC)"
  ) +
  theme(
    panel.background = element_blank(),
    plot.background = element_rect(fill="transparent", color=NA),
    panel.border = element_blank(),#rect(fill="transparent", color="gray50"),
    panel.grid.major.x = element_line(color = "gray95"),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 1),
    legend.position = "NA"
  )

dev.off()


# Note: for Figures 9 and 10 scroll down to Appendix C.





################################################################################
################################################################################
#
# Appendix A ___________________________________________________________________
# Survey data: TC-mapping heterogeneity among SFI reviewers
#
#
#
#
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
  "other / prefer not to say"

d <- melt(s$populationDemographics$program, id.vars = "class")
d$facet <- "reviewed for"
d$class[d$class == "only Industry Fellowship"] <-
  "Industry Fellowship"
d$class[d$class == "only Investigators Programme"] <-
  "Investigators Programme"
d$class[d$class == "both"] <- "both"
d$class[d$class == "neither/non-response"] <-
  "neither"

demo <- rbind(a, b, c, d)
rm(a, b, c, d)

demo$class <- factor(x = demo$class, levels = unique(demo$class))
demo$facet <- factor(x = demo$facet, levels = unique(demo$facet))
demo$value <- demo$value * 100
#demo$variable <- factor(x = demo$class, levels = unique(demo$class))


figureParameters <- list(
  filename = paste0("./outputGraphics/Fig11.", exportFormat),
  width = 1600,
  height = 800,
  units = "px",
  res = 300
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

ggplot(demo, aes(x = class, y = value, fill = variable)) + 
  geom_col(
    position = position_dodge2(reverse = FALSE, width = NULL, padding = 0),
    width = 0.5, color = "black", size = 0.5
  ) +
  #scale_x_discrete(limits = rev) +
  #facet_wrap(facets = demo$facet, nrow = 1, scales = "free_x") +
  facet_grid(cols = vars(demo$facet), scales = "free_x", space = "free") +
  ylab("relative frequency") +
  #scale_fill_discrete(
  scale_fill_viridis_d(
    name = "", begin = 0.85, end = 0.4, option = "A",# begin = 0.85, end = 0.1,
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


################################################################################
################################################################################
#
# Appendix B ___________________________________________________________________
th <- qbeta(1:4 / 5, shape1 = 2, shape2 = 1) * 100
df <- rbind(
  data.frame(
    x = s$i$q34,
    th = rep(
      "1st threshold: between\n'very low quality' and 'low quality'",
      times = length(s$i$q34)
  )),
  data.frame(
  x = s$i$q33,
  th = rep(
    "4th threshold: between\n'high quality' and 'outstanding'",
    times = length(s$i$q33)
  ))
)


figureParameters <- list(
  filename = paste0("./outputGraphics/Fig12.", exportFormat),
  width = 1250,
  height = 700,
  units = "px",
  res = 300
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}

ggplot(df, aes(x = x, fill = th)) +
  geom_histogram(breaks = 0:10 * 10, color = "black", position = "dodge") +
  #geom_vline(xintercept = th, linetype = 2, color = "black") +
  facet_grid(cols = vars(th)) +
  scale_fill_viridis_d(name = "", begin = 0.85, end = 0.4, option = "A") +
  scale_x_continuous(breaks = 0:10 * 10, labels = function(x){paste0(x, "%")}) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "quality percentage", y = "frequency") +
  theme(
    plot.margin = margin(0, 0, 0, 0, "pt"),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "gray95"),
    legend.position = "NA",
    legend.background = element_rect(fill = "gray95"),
    strip.background = element_blank(),#element_rect(fill = "gray95"),
    #axis.title = element_blank(),
    axis.line.y = element_line(colour = "black"),
    axis.line.x = element_blank(),
    axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1, size = 7))

dev.off()





################################################################################
################################################################################
# Appendix C ___________________________________________________________________
# Exploring parameter space
#
#
#
# ______________________________________________________________________________
# Number of topics/attributes (N)
#
focusVariable = "nTopics"
focusVariableLab = "number of topics (N)"
#focusVariable = ""

closeToBaseline <- apply(
  X = ri,
  MARGIN = 1,
  FUN = function(x) {
    ifelse(
      any(x[variables[variables != focusVariable]] !=
            as.data.frame(t(baseline[variables != focusVariable]))),
      return(FALSE),
      return(TRUE)
    )
  }
)
rii <- ri[closeToBaseline,]

fig <- ggpubr::ggarrange(
  plotParameter(
    data = rii,
    dep = "ICC", deplabel = "IRR (ICC)",
    indep = "TCMswapping",
    indeplabel = "",
    facetby = focusVariable,
    facetlabel = focusVariableLab
  ),
  plotParameter(
    data = rii,
    dep = "spearmanRho", deplabel = "IRR (Spearman)",
    indep = "TCMswapping",
    indeplabel = "TC-mapping heterogeneity (ρ)",
    facetby = focusVariable,
    facetlabel = ""
  ),
  ncol = 1, hjust = -1, align = "hv"
)

figureParameters <- list(
  filename = paste0("./outputGraphics/Fig13.", exportFormat),
  width = 1500, height = 1200, res = 300, units = "px"
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}
print(fig)
dev.off()





# ______________________________________________________________________________
# Number of evaluation criteria (C)
#
focusVariable = "nCriteria"
focusVariableLab = "number of evaluation criteria (C)"

closeToBaseline <- apply(
  X = ri,
  MARGIN = 1,
  FUN = function(x) {
    ifelse(
      any(x[variables[variables != focusVariable]] !=
            as.data.frame(t(baseline[variables != focusVariable]))),
      return(FALSE),
      return(TRUE)
    )
  }
)
rii <- ri[closeToBaseline,]


fig <- ggpubr::ggarrange(
  plotParameter(
    data = rii,
    dep = "ICC", deplabel = "IRR (ICC)",
    indep = "TCMswapping",
    indeplabel = "",
    facetby = focusVariable,
    facetlabel = focusVariableLab
  ),
  plotParameter(
    data = rii,
    dep = "spearmanRho", deplabel = "IRR (Spearman)",
    indep = "TCMswapping",
    indeplabel = "TC-mapping heterogeneity (ρ)",
    facetby = focusVariable,
    facetlabel = ""
  ),
  ncol = 1, hjust = -1, align = "hv"
)

figureParameters <- list(
  filename = paste0("./outputGraphics/Fig14.", exportFormat),
  width = 1500, height = 1200, res = 300, units = "px"
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}
print(fig)
dev.off()





# ______________________________________________________________________________
# Reviewer error (E)
#
focusVariable = "reviewerError"
focusVariableLab = "reviewer error (\u0190)"

closeToBaseline <- apply(
  X = ri,
  MARGIN = 1,
  FUN = function(x) {
    ifelse(
      any(x[variables[variables != focusVariable]] !=
            as.data.frame(t(baseline[variables != focusVariable]))),
      return(FALSE),
      return(TRUE)
    )
  }
)
rii <- ri[closeToBaseline,]

fig <- ggpubr::ggarrange(
  plotParameter(
    data = rii,
    dep = "ICC", deplabel = "IRR (ICC)",
    indep = "TCMswapping",
    indeplabel = "",
    facetby = focusVariable,
    facetlabel = focusVariableLab
  ),
  plotParameter(
    data = rii,
    dep = "spearmanRho", deplabel = "IRR (Spearman)",
    indep = "TCMswapping",
    indeplabel = "TC-mapping heterogeneity (ρ)",
    facetby = focusVariable,
    facetlabel = ""
  ),
  ncol = 1, hjust = -1, align = "hv"
)

figureParameters <- list(
  filename = paste0("./outputGraphics/Fig15.", exportFormat),
  width = 1500, height = 1200, res = 300, units = "px"
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}
print(fig)
dev.off()





# ______________________________________________________________________________
# Reviewer bias diversity (λ)
#
focusVariable = "reviewerBiasDiversity"
focusVariableLab = "diversity in reviewer bias (λ)"

closeToBaseline <- apply(
  X = ri,
  MARGIN = 1,
  FUN = function(x) {
    ifelse(
      any(x[variables[variables != focusVariable]] !=
            as.data.frame(t(baseline[variables != focusVariable]))),
      return(FALSE),
      return(TRUE)
    )
  }
)
rii <- ri[closeToBaseline,]

fig <- ggpubr::ggarrange(
  plotParameter(
    data = rii,
    dep = "ICC", deplabel = "IRR (ICC)",
    indep = "TCMswapping",
    indeplabel = "",
    facetby = focusVariable,
    facetlabel = focusVariableLab
  ),
  plotParameter(
    data = rii,
    dep = "spearmanRho", deplabel = "IRR (Spearman)",
    indep = "TCMswapping",
    indeplabel = "TC-mapping heterogeneity (ρ)",
    facetby = focusVariable,
    facetlabel = ""
  ),
  ncol = 1, hjust = -1, align = "hv"
)

figureParameters <- list(
  filename = paste0("./outputGraphics/Fig16.", exportFormat),
  width = 1500, height = 1200, res = 300, units = "px"
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}
print(fig)
dev.off()





# ______________________________________________________________________________
# Grading language heterogeneity (h)
#
focusVariable = "GLdiversity"
focusVariableLab = "diversity in grading language interpretations (h)"

closeToBaseline <- apply(
  X = ri,
  MARGIN = 1,
  FUN = function(x) {
    ifelse(
      any(x[variables[variables != focusVariable]] !=
            as.data.frame(t(baseline[variables != focusVariable]))),
      return(FALSE),
      return(TRUE)
    )
  }
)
rii <- ri[closeToBaseline,]

fig <- ggpubr::ggarrange(
  plotParameter(
    data = rii,
    dep = "ICC", deplabel = "IRR (ICC)",
    indep = "TCMswapping",
    indeplabel = "",
    facetby = focusVariable,
    facetlabel = focusVariableLab
  ),
  plotParameter(
    data = rii,
    dep = "spearmanRho", deplabel = "IRR (Spearman)",
    indep = "TCMswapping",
    indeplabel = "TC-mapping heterogeneity (ρ)",
    facetby = focusVariable,
    facetlabel = ""
  ),
  ncol = 1, hjust = -1, align = "hv"
)

figureParameters <- list(
  filename = paste0("./outputGraphics/Fig17.", exportFormat),
  width = 1500, height = 1200, res = 300, units = "px"
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}
print(fig)
dev.off()





# ______________________________________________________________________________
# Attribute correlation (r)
#
focusVariable = "attributeCorr"
focusVariableLab = "attribute correlation (r)"

closeToBaseline <- apply(
  X = ri,
  MARGIN = 1,
  FUN = function(x) {
    ifelse(
      any(x[variables[variables != focusVariable]] !=
            as.data.frame(t(baseline[variables != focusVariable]))),
      return(FALSE),
      return(TRUE)
    )
  }
)
rii <- ri[closeToBaseline,]


fig <- ggpubr::ggarrange(
  plotParameter(
    data = rii,
    dep = "ICC", deplabel = "IRR (ICC)",
    indep = "TCMswapping",
    indeplabel = "",
    facetby = focusVariable,
    facetlabel = focusVariableLab
  ),
  plotParameter(
    data = rii,
    dep = "spearmanRho", deplabel = "IRR (Spearman)",
    indep = "TCMswapping",
    indeplabel = "TC-mapping heterogeneity (ρ)",
    facetby = focusVariable,
    facetlabel = ""
  ),
  ncol = 1, hjust = -1, align = "hv"
)

figureParameters <- list(
  filename = paste0("./outputGraphics/Fig18.", exportFormat),
  width = 1100, height = 1200, res = 300, units = "px"
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}
print(fig)
dev.off()


figureParameters <- list(
  filename = paste0("./outputGraphics/Fig9.", exportFormat),
  width = 1100, height = 700, res = 300, units = "px"
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}
plotParameter(
  data = rii,
  dep = "ICC", deplabel = "inter-rater reliability (ICC)",
  indep = "TCMswapping",
  indeplabel = "TC-mapping heterogeneity (ρ)",
  facetby = focusVariable,
  facetlabel = focusVariableLab
)
dev.off()





# ______________________________________________________________________________
# Grading scale granularity (s)
#
focusVariable = "gradingScale"
focusVariableLab = "grading scale granularity (s)"

closeToBaseline <- apply(
  X = ri,
  MARGIN = 1,
  FUN = function(x) {
    ifelse(
      any(x[variables[variables != focusVariable]] !=
            as.data.frame(t(baseline[variables != focusVariable]))),
      return(FALSE),
      return(TRUE)
    )
  }
)
rii <- ri[closeToBaseline,]

fig <- ggpubr::ggarrange(
  plotParameter(
    data = rii,
    dep = "ICC", deplabel = "IRR (ICC)",
    indep = "TCMswapping",
    indeplabel = "",
    facetby = focusVariable,
    facetlabel = focusVariableLab
  ),
  plotParameter(
    data = rii,
    dep = "spearmanRho", deplabel = "IRR (Spearman)",
    indep = "TCMswapping",
    indeplabel = "TC-mapping heterogeneity (ρ)",
    facetby = focusVariable,
    facetlabel = ""
  ),
  ncol = 1, hjust = -1, align = "hv"
)

figureParameters <- list(
  filename = paste0("./outputGraphics/Fig19.", exportFormat),
  width = 1500, height = 1200, res = 300, units = "px"
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}
print(fig)
dev.off()


figureParameters <- list(
  filename = paste0("./outputGraphics/Fig10.", exportFormat),
  width = 1500, height = 700, res = 300, units = "px"
)
if(exportFormat == "png") {do.call(png, figureParameters)} else {
  do.call(tiff, figureParameters)}
plotParameter(
  data = rii,
  dep = "ICC", deplabel = "inter-rater reliability (ICC)",
  indep = "TCMswapping",
  indeplabel = "TC-mapping heterogeneity (ρ)",
  facetby = focusVariable,
  facetlabel = focusVariableLab
)
dev.off()





################################################################################
################################################################################
#
# Other:
# On TC-mapping heterogeneity __________________________________________________

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

# We also calculate it for each evaluation criterion separately...
for (c in 1:3) {
  print(paste0(
    "Criterion ", c, ": average norm. Hamming dist.: ",
    round(mean(apply(
      X = diff,
      MARGIN = 1,
      FUN = function(x) twdis(
        a = TCM[[x[1]]][,c], b = TCM[[x[2]]][,c], normalized = TRUE)
    )), digits = 3)
  ))
}





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
for (rep in 1:10000) {
  
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
