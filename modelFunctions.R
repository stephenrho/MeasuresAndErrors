# modelFunctions.R
# Author: Stephen Rhodes (rhodessp at missouri.edu)
# License: GNU GPL v3.0

# Contains functions used to generate data according to a SDT or THT account of recognition and to calculate measures (d', Pr, A') from simulated data.
# Also contains other functions used for figures in the manuscript
# Rhodes, Cowan, Parra, Logie "Interaction Effects on Common Measures of Sensitivity: Choice of Measure, Type I Error and Power"

direc <- '~/github/MeasuresAndErrors/'
setwd(direc)

### Model Predictions ----
SDT <- function(d, k, s = 1){
  f = pnorm(-d/2 - k)
  h = pnorm((d/2 - k)/(1/s))
  return(c(f = f, h = h))
}

THT <- function(Pr, Pt = Pr, Pn = Pr, Br){
  f = (1 - Pn)*Br
  h = Pt + (1 - Pt)*Br
  return(c(f = f, h = h))
}

### Measures -----
dprime <- function(hit, fa, adjust = F) {
  if (adjust == T) {
    hit = ifelse(hit < .99, hit, .99)
    hit = ifelse(hit > .01, hit, .01)
    fa = ifelse(fa > .01, fa, 0.01)
    fa = ifelse(fa < .99, fa, 0.99)
  }
  qnorm(hit) - qnorm(fa)
}

aprime <-function(hit, fa) {
  a<-1/2+((hit-fa)*(1+hit-fa) /
            (4*hit*(1-fa)))
  b<-1/2-((fa-hit)*(1+fa-hit) /
            (4*fa*(1-hit)))
  a[fa>hit]<-b[fa>hit]
  a[fa==hit]<-.5
  a
}

Pr <- function(hit, fa) { # dont really need a function...
  hit - fa
}

### OTHER USEFUL FUNCTIONS -----
# Convert Between Measures
Pr.to.dp <- function(Pr, Br){
  h = Pr + (1 - Pr)*Br
  f = (1 - Pr)*Br
  h = ifelse(h > 1, .99, h)
  f = ifelse(f < 0, .01, f)
  
  d = qnorm(h) - qnorm(f)
  return(d)
}

dp.to.Pr <- function(d, k){
  f = pnorm(-k)
  h = pnorm(d - k)
  
  Pr = h - f
  return(Pr)
}

SDT.ROC <- function(f, d, s = 1){
  pnorm((d + qnorm(f))/(1/s))
  #pnorm(qnorm(f) + d)
}

THT.ROC <- function(f, Pr){
  h = Pr + f
  return(ifelse(h < 1, h , NA))
}

# Predicted hits and false alarms for 2 x 2 design

ratesTHT <- function(Pr_0 = .6, Pr_1 = .1, Pr_2 = -.3, Pr_3 = 0, Br_0 = .5, Br_1 = 0, Br_2 = 0){
  # create design matrix and grouping variables
  X = matrix(data = c(1,1,1,1, -1,-1,1,1, -1,1,-1,1), ncol = 3)
  X <- cbind(X, X[,2]*X[,3])
  
  groups = factor(c(1,1,2,2))
  conds = factor(c(1,2,1,2))
  
  # generate parameters and expected rates
  Prs = X %*% c(Pr_0, Pr_1, Pr_2, Pr_3)
  Brs = X %*% c(Br_0, Br_1, Br_2, 0)
  
  Prs[Prs > 1] <- 1; Prs[Prs < 0] <- 0
  Brs[Brs > 1] <- 1; Brs[Brs < 0] <- 0
  
  h = c(); f = c()
  for (row in 1:nrow(X)){
    fh = THT(Pr = Prs[row], Br = Brs[row])
    h = c(h, fh[2])
    f = c(f, fh[1])
  }
  
  mat <- cbind(groups, conds, Prs, Brs, h, f)
  colnames(mat) <- c('Group', 'Cond', 'Pr', 'Br', 'hr', 'far')
  rownames(mat) <- 1:nrow(mat)
  return(as.data.frame(mat))
}

ratesSDT <- function(d_0 = 2, d_1 = .4, d_2 = -.5, d_3 = 0, k_0 = 2/2, k_1 = 0, k_2 = 0, s=1){
  # create design matrix and grouping variables
  X = matrix(data = c(1,1,1,1, -1,-1,1,1, -1,1,-1,1), ncol = 3)
  X <- cbind(X, X[,2]*X[,3])
  
  groups = factor(c(1,1,2,2))
  conds = factor(c(1,2,1,2))
  
  # generate parameters and expected rates
  ds = X %*% c(d_0, d_1, d_2, d_3)
  ks = X %*% c(k_0, k_1, k_2, 0)
  
  h = c(); f = c()
  for (row in 1:nrow(X)){
    fh = SDT(d = ds[row], k = ks[row], s = s)
    h = c(h, fh[2])
    f = c(f, fh[1])
  }
  
  mat <- cbind(groups, conds, ds, ks, h, f)
  colnames(mat) <- c('Group', 'Cond', 'd', 'k', 'hr', 'far')
  rownames(mat) <- 1:nrow(mat)
  return(as.data.frame(mat))
}

plot2x2ROC <- function(data, ROCs = c('d', 'Pr'), s = 1, cols = c('mediumpurple1', 'springgreen2'), pchs = c(22, 24), plotAxes = F, leg = F, lineWidth = 1, pointSize = 1){
  # settings
  dlty = 2
  Prlty = 1
  fs = seq(0,1,0.001)
  
  plot(1000, xlim = c(0,1), ylim = c(0,1), ylab='', xlab='', axes = F)
  # add ROCs
  if (!is.null(ROCs)){
    for (roc in ROCs){
      if (roc == 'd'){
        lapply(1:nrow(data), function(i) lines(fs, SDT.ROC(f = fs, d = data$d[i], s = s), col = 'grey', lty = dlty, lwd=lineWidth))
      }
      if (roc == 'Pr'){
        lapply(1:nrow(data), function(i) lines(fs, THT.ROC(f = fs, Pr = data$Pr[i]), col = 'lightgrey', lty = Prlty, lwd=lineWidth))
      }
    }
  }
  # add points
  with(data, points(far, hr, pch = pchs[Group], col = 'black', bg = cols[Cond], cex=pointSize))
  box()
  if (plotAxes){
    axis(side = 1, at = seq(0, 1, .2))
    mtext(text = 'false-alarm rate', side = 1, line = 2.5)
    axis(side = 2, at = seq(0, 1, .2))
    mtext(text = 'hit rate', side = 2, line = 2.5)
  }
  if (leg){
    legend('bottomright', c('Group 1', 'Group 2', 'Cond 1', 'Cond 2'), 
           pch = c(22, 24, 16, 16), col = c('black', 'black', cols), cex = 1)
  }
}
