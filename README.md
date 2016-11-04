# Simulation code for Rhodes et al. “Interaction Effects on Common Measures of Sensitivity: Choice of Measure, Type I Error and Power”

This repository contains `R` code use to simulate type I error rates and power for interaction on single point estimates of sensitivity in detection/ recognition paradigms. A simple 2 x 2 design (group by condition) is simulated. Simulated data sets conformed to the expectations of a signal detection theory (SDT) or two-high threshold (THT) model and three common estimators of sensitivity were calculated with the simulated data sets (d’, A’, and Pr or hits minus false alarms). 

`modelFunctions.R` contains functions for implementing the predictions of SDT and THT accounts of discrimination as well as functions for calculating our measures of interest. 

`simFunctions.R` contains functions used to generate simulated data sets according to a generative model and specific settings for its underlying parameters (determining the sensitivity and bias of observers in different groups and conditions), calculate the proportion of significant interactions and return the estimated type I error/ power. `runSims` is the main function here and takes a matrix of parameter settings for a given generative model and returns the matrix with rates (of sig. interactions) for each of the three measures.

`runSims.R` calls the above two scripts to perform the simulations themselves. Each simulation is reported either in the main manuscript or in supplementary material. 

`plotSims.R` is used throughout the manuscript and supplement to plot the results of simulations.

`logit.R` applies a logit mixed model to the first set of GEV-SDT simulations reported to show that changing the scale of a THT outcome measure (proportion correct) can reduce the type I error rate when there is no variation in bias. Discussed in the ‘Recommendations’ section.

The results folder contains the tables of raw output from `runSims.R` as well as the saved simulated data sets from the first set of simulations used by `logit.R`. 

Any questions please email rhodessp (at) missouri.edu
