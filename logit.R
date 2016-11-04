# logit.R
# Author: Stephen Rhodes (rhodessp at missouri.edu)
# License: GNU GPL v3.0

# Implements a logit analysis of the first set of simulations reported in 
# Rhodes, Cowan, Parra, Logie "Interaction Effects on Common Measures of Sensitivity: Choice of Measure, Type I Error and Power"

library(lme4)

direc <- '~/github/MeasuresAndErrors/'
setwd(direc)

load('results/sim1/sim1_EVSDT.RData')

head(sim1_EVSDT)

# sim1_EVSDT gives each combination of parameter values
# sim1_EVSDT_[n] gives the simulated data sets for row n in sim1_EVSDT

# column to add to
sim1_EVSDT$BxW.logit <- NA
for (i in 1:nrow(sim1_EVSDT)){
  # retrieve simulation
  tmpArray = get(x = paste0('sim1_EVSDT_', i))
  # loop through data sets 
  intPvals <- c()
  for (dset in 1:dim(tmpArray)[3]){
    tmpData <- tmpArray[,,dset]
    tmpData <- as.data.frame(tmpData)
    tmpData$nCor <- with(tmpData, hits + N_t - fas)
    # make everything into a factor
    tmpData$B <- as.factor(tmpData$B)
    tmpData$W <- as.factor(tmpData$W)
    tmpData$S <- as.factor(tmpData$S)
    # run logit model
    res <- glmer(cbind(nCor, N_t*2 - nCor) ~ B*W + (1|S), data = as.data.frame(tmpData), family = binomial(link = logit ))
    # extract p value
    p <- summary(res)$coefficients[4,4]
    # add
    intPvals <- c(intPvals, p)
  }
  # calculate type i error
  errorRate <- sum((intPvals < 0.05))/length(intPvals)
  # add to sim1_EVSDT
  sim1_EVSDT$BxW.logit[i] <- errorRate
}

rm(list = c('tmpArray', 'tmpData'))

write.table(sim1_EVSDT, 'results/tables/sim1_EVSDT_logit')

# sim1_EVSDT$BxW.logit

# Compare to other error rates
logVsPr <- with(sim1_EVSDT, BxW.logit - BxW.Pr)

logVsdp <- with(sim1_EVSDT, BxW.logit - BxW.dp)

# > 0 higher error rates for logit
# < 0 lower error rates for logit
hist(logVsPr)
mean(logVsPr)
range(logVsPr)

hist(logVsdp)
mean(logVsdp)
range(logVsdp)

with(sim1_EVSDT, plot(BxW.logit, BxW.Pr, xlim=c(0,1), ylim=c(0,1)))
abline(0,1)

with(sim1_EVSDT, plot(BxW.logit, BxW.dp, xlim=c(0,1), ylim=c(0,1)))
abline(0,1)

