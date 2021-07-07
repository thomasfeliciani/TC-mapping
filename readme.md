# Topics-criteria mapping and inter-rater reliability
These scripts run a simulation model of inter-rater reliability (IRR) and analyze the results thereof. Each run simulates an expert panel (such as a peer review panel) whose members are tasked with the evaluation of a set of submissions. Each panel member evaluates the submissions by assigning them a grade in a specified grading scale.  Various factors affect the IRR among the panel members, generally relating to the features of the panel, its members and the rules of the evaluation process. This simulation model considers some of these factors:
* Noise in the evaluation by the panel members;
* Interpersonal differences in biases influencing the evaluation;
* Interpersonal differences in the interpretation of the grading scale;
* Interpersonal differences in the choice of aspects of a proposal on which to base one’s judgment;
* Granularity of the grading scale.
We use this simulation model to study whether, how and under what conditions (a combination of) these factors affect IRR in an expert panel.

This code runs in [R 4.1.0](https://www.r-project.org/) 
Follows a description of the function of each script.
# util.r
This script contains utility functions that are ancillary to the simulation model.

# simulation.r
_Requires libraries:_ “faux” _and_ “irr”_._

_Sources_ “util.r”_._


# battery.r
_Requires libraries:_ “compiler”, “parallel” _and_ “doSNOW”_._

_Sources_ “util.r”_._

_Loads_ “./data/pTCM.RData”_._



# results.r
_Requires libraries:_ “ggplot2”, “reshape” _and_ “ggpubr”_._

_Sources_ “util.r”_._

_Loads the simulation results from:_ “./data/unshareable/survey.RData”_._



