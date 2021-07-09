# Topics-criteria mapping and inter-rater reliability
These scripts run a simulation model of inter-rater reliability (IRR) and analyze the results. Each run simulates an expert panel (such as a peer review panel) whose members are tasked with the evaluation of a set of submissions (such as grant proposals). Each panel member evaluates the submissions by assigning them a grade in a specified grading scale. Various factors affect the IRR among the panel members, generally relating to the features of the panel, its members and the rules of the evaluation process. This simulation model considers some of these factors:
* Noise in the evaluation by the panel members;
* Interpersonal differences in biases influencing the evaluation;
* Interpersonal differences in the interpretation of the grading scale;
* Interpersonal differences in the choice of aspects of a submission on which to base one’s judgment;
* Granularity of the grading scale.
We use this simulation model to study whether, how and under what conditions (a combination of) these factors affect IRR in an expert panel.

This code runs in [R 4.1.0](https://www.r-project.org/) (R Core Team, 2021).

Follows a description of the function of each script. 

## util.r
This script contains utility functions that are ancillary to the simulation model.

## simulation.r
_Requires libraries:_ “faux” _and_ “irr”_._

_Sources_ “util.r”_._

_Loads_ “./data/pTCM.RData”_._


This script is the core of the simulation model, as it defines the function that runs one simulation.

### Outline of a simulation run
Each simulation starts with the creation of:
* A set of submissions and their related attributes;
* A grading scale (i.e. a set of intervals mapped onto an underlying continuous quality scale);
* A template mapping of aspects of the submissions onto the evaluation criteria against which the submissions are to be evaluated against.

Then, the simulation assigns the following to each panel member:
* Some degree of competence (which affects noise in their evaluation);
* Some degree of bias: this implements how lenient or strict each panel member is;
* An interpretation of the grading scale: this is implemented as interpersonal variation in mapping the grading scale onto the underlying continuous quality scale;
* A unique choice of topics to consider when evaluating the various evaluation criteria. Interpersonal variation here is achieved by rewiring the template mapping of aspects of the submissions onto the evaluation criteria.
Based on these reviewers attributes, the simulation model calculates for each panel member their opinion of each of the submissions. The opinion is then converted into a grade in the prescribed grading scale.
The simulation ends with the calculation of the outcome variables. IRR is operationalized in different ways, including an intra-class correlation coefficient and Spearman’s rank correlation coefficient.

### Other information
The script includes an example call to the function that runs the simulation (see the very end of the script).

The file “pTCM.RData” contains a 12-by-3 matrix with the probability weights for the creation of the template mapping of (twelve) submission aspects onto (three) evaluation criteria. See Section “Empirical data” for more details.


## battery.r
_Requires libraries:_ “compiler”, “parallel” _and_ “doSNOW”_._

_Sources_ “util.r”_._

_Loads_ “./data/pTCM.RData”_._


This script defines all points of the parameter space that are to be simulated. These are saved in a dataframe named “battery”, where each row is a unique parameter configuration and each column is one of the parameters of the simulation model that are going to be explored. For each of the parameter configurations, this script then runs a specified number of independent simulation runs; bundles the parameter configuration and outcome variables of each, and saves everything to file: “./output/ri.RData”.

Running this script might take several hours, depending on the area of the parameter space explored and on the number of repetitions per configuration.


## results.r
_Requires libraries:_ “ggplot2”_,_ “reshape” _and_ “ggpubr”_._

_Sources_ “util.r”_._

_Loads datasets:_ “./data/pTCM.RData” _and_ “./output/ri.RData”_._


This script reproduces descriptive statistics and plots to explore the results of:
* The simulation model (from “ri.RData”);
* A survey of peer reviewers (see next section). Because the survey cannot be shared (it is not included in this repository), descriptive statistics and plots based on the survey cannot be replicated. If sourced, the script will attempt to load the survey data from “./data/unshareable/survey.RData”, fail, and return a warning message.

## Empirical data
Empirical data is derived from a survey of peer reviewers from a national science funding institution. The survey documentation is public and includes its questionnaire (Shankar et al. 2021 - [DOI](https://doi.org/10.6084/m9.figshare.13651058.v1)). The microdata, however, are not shareable as per agreement with the funding institution and is thus not included in this repository.

For cross-reference, these are the survey items that are used in the script, and what for:
* _“Figure A1”_ plots the demographics of survey respondents, comparing it to the demographics of the population of reviewers from the funding institution. This is based on the survey questions Q1 (gender), Q2 (academic background), Q3 (contry of respondent´s institution), Q8 and Q16 (funding program for which the respondent has reviewed).
* _“Figure B1”_ plots the responses to questions Q33-34. These are used to infer peer reviewers´ interpretation of a grading scale and degree of interpersonal variation in this regard.
* The script “results.r” uses responses to Q27a-l to calculate how, on average, peer reviewers map twelve aspects of a submission onto three evaluation criteria in use at the funding institution they reviewed for. These responses, appropriately aggregated, are then saved to file: “./data/pTCM.RData” - which is then used by the simulation model in various ways (see Sections “simulation.r” and “results.r”).


## Acknowledgements
This material is based upon works supported by the Science Foundation Ireland under Grant No.17/SPR/5319.


## References
* R Core Team (2021). _R: A language and environment for statistical computing_. R Foundation for Statistical Computing, Vienna, Austria. https://www.R-project.org/.
* Shankar, Kalpana; Luo, Junwen; Ma, Lai; Lucas, Pablo; Feliciani, Thomas (2021). _SPRING 2020 survey: peer review of grant proposals_. figshare. Dataset. https://doi.org/10.6084/m9.figshare.13651058.v1

