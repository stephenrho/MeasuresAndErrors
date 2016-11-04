# simFunctions.R
# Author: Stephen Rhodes (rhodessp at missouri.edu)
# License: GNU GPL v3.0

# Functions to run simulations in:
# Rhodes, Cowan, Parra, Logie "Interaction Effects on Common Measures of Sensitivity: Choice of Measure, Type I Error and Power"

direc <- '~/github/MeasuresAndErrors/'
setwd(direc)

source('modelFunctions.R')

### Simulate SDT data ----
simSDT <- function(nReps, N_s, N_t, d_0, d_1, 
                   d_2, d_3, d_SD, k_0, k_1, 
                   k_2, k_3, k_SD, S = 1){
  
  colNams <- c('B', 'S', 'W', 'N_t', 'hits', 'fas')
  
  # create array
  dataArray = array(dim = c(N_s*4, length(colNams), nReps))
  
  for (r in 1:nReps){
    # create matrix to run sim and store results
    tempMat = cbind(
      b = rep(c(1,2), each = 2*N_s), # between subjects
      s = c(rep(1:N_s, times = 2), rep((N_s+1):(N_s*2), times = 2)), # subject id
      w = rep(c(1,2), each = N_s), # within subjects
      N_t = N_t,
      hits = 0, fas = 0
    )
    
    # sample participant effects
    d_sEffs = rnorm(n = N_s*2, mean = 0, sd = d_SD)
    k_sEffs = rnorm(n = N_s*2, mean = 0, sd = k_SD)
    
    # populate matrix
    for (i in 1:nrow(tempMat)){
      # parameter values
      d = with(data.frame(t(tempMat[i,])), 
               d_0 + d_1*c(-1,1)[b] + d_2*c(-1,1)[w] + d_3*c(-1,1)[b]*c(-1,1)[w] + d_sEffs[s])
      k = with(data.frame(t(tempMat[i,])),
               k_0 + k_1*c(-1,1)[b] + k_2*c(-1,1)[w] + k_3*c(-1,1)[b]*c(-1,1)[w] + k_sEffs[s])
      # generate data for t trials
      fh = SDT(d = d, k = k, s = S) # calculate predicted rates
      hits = rbinom(n = N_t, size = 1, prob = fh[2]) # sample
      fas = rbinom(n = N_t, size = 1, prob = fh[1])
      # save
      tempMat[i, 'hits'] <- sum(hits)
      tempMat[i, 'fas'] <- sum(fas)
    }
    dataArray[,,r] <- tempMat
  }
  colnames(dataArray) = colNams
  return(dataArray)
}

### Simulate Two High Threshold data ----
simTHT <- function(nReps, N_s, N_t, Pr_0, Pr_1, 
                   Pr_2, Pr_3, Pr_SD, Br_0, Br_1, 
                   Br_2, Br_3, Br_SD){
  
  colNams <- c('B', 'S', 'W', 'N_t', 'hits', 'fas')
  
  # create array
  dataArray = array(dim = c(N_s*4, length(colNams), nReps))
  
  for (r in 1:nReps){
    # create matrix to run sim and store results
    tempMat = cbind(
      b = rep(c(1,2), each = 2*N_s), # between subjects
      s = c(rep(1:N_s, times = 2), rep((N_s+1):(N_s*2), times = 2)), # subject id
      w = rep(c(1,2), each = N_s), # within subjects
      N_t = N_t,
      hits = 0, fas = 0
    )
    # sample participant effects
    Pr_sEffs = rnorm(n = N_s*2, mean = 0, sd = Pr_SD)
    Br_sEffs = rnorm(n = N_s*2, mean = 0, sd = Br_SD)
    
    # populate matrix
    for (i in 1:nrow(tempMat)){
      # predictions
      Pr = with(data.frame(t(tempMat[i,])),
                Pr_0 + Pr_1*c(-1,1)[b] + Pr_2*c(-1,1)[w] + Pr_3*c(-1,1)[b]*c(-1,1)[w] + Pr_sEffs[s])
      Br = with(data.frame(t(tempMat[i,])),
                Br_0 + Br_1*c(-1,1)[b] + Br_2*c(-1,1)[w] + Br_3*c(-1,1)[b]*c(-1,1)[w] + Br_sEffs[s])
      # make sure between 0 and 1
      Pr = max(0, min(Pr, 1))
      Br = max(0, min(Br, 1))
      # generate data for t trials
      fh = THT(Pr = Pr, Br = Br)
      hits = rbinom(n = N_t, size = 1, prob = fh[2])
      fas = rbinom(n = N_t, size = 1, prob = fh[1])
      # save
      tempMat[i, 'hits'] <- sum(hits)
      tempMat[i, 'fas'] <- sum(fas)
    }
    dataArray[,,r] <- tempMat
  }
  colnames(dataArray) = colNams
  return(dataArray)
}

### Calculate Measures ----
calcMeasures <- function(array){
  arrayDims = dim(array)
  outArray = array(dim = arrayDims + c(0,3,0))
  for(i in 1:arrayDims[3]){
    N_t = array[,'N_t',i]
    h = array[,'hits',i]/N_t
    f = array[,'fas',i]/N_t
    dp = dprime(hit = h, fa = f, adjust = T)
    Pr = Pr(hit = h, fa = f)
    ap = aprime(hit = h, fa = f)
    outArray[,,i] <- cbind(array[,,i], dp, Pr, ap)
  }
  colnames(outArray) <- c(colnames(array), 'dp', 'Pr', 'Ap')
  return(outArray)
}

# DA <- simSDT(nReps = 10)
# DA <- calcMeasures(DA)
# DA

#### ANOVA/ P-value functions ----
# function to extract p values from mixed aov
aov.p <- function(obj){
  sum = summary(obj)
  b = sum[[1]][[1]]$'Pr(>F)'[1]
  w = sum[[2]][[1]]$'Pr(>F)'[1]
  i = sum[[2]][[1]]$'Pr(>F)'[2]
  return(c(b,w,i))
}

extractPs <- function(dataArray){
  Ntests = dim(dataArray)[3]
  # make matrix to store results
  pvalues = matrix(nrow = Ntests, ncol = 3*3)
  for (t in 1:Ntests){
    temp.df = as.data.frame(dataArray[,,t]) # select data set
    temp.df$B = as.factor(temp.df$B)
    temp.df$W = as.factor(temp.df$W)
    temp.df$S = as.factor(temp.df$S)
    # do anovas
    anov.dp = aov(dp ~ B*W + Error(S/W), data=temp.df)
    anov.Ap = aov(Ap ~ B*W + Error(S/W), data=temp.df)
    anov.Pr = aov(Pr ~ B*W + Error(S/W), data=temp.df)
    # extract ps
    p.dp = aov.p(anov.dp)
    p.Ap = aov.p(anov.Ap)
    p.Pr = aov.p(anov.Pr)
    # add to mat
    pvalues[t,] = c(p.dp, p.Ap, p.Pr)
  }
  colnames(pvalues) <- c('B.dp', 'W.dp', 'BxW.dp', 
                         'B.Ap', 'W.Ap', 'BxW.Ap',
                         'B.Pr', 'W.Pr', 'BxW.Pr')
  return(pvalues)
}

### WRAPPER FUNCTION FOR RUNNING SIMS ----
runSims <- function(mat, model, Nreps = 1000, saveDat = T){
  # Provide a matrix of coefficients for simulation function
  # Function used depends on model arg
  # If saveDat then the simulated data sets are saved to objects
  # pvalues for main effects and interactions are added to mat for each measure and returned
  
  matName <- deparse(substitute(mat)) # used for saving
  # columns to hold type i error rate
  mat = cbind(mat, matrix(NA, nrow = nrow(mat), ncol = 9)) # expand matrix to hold p-vals
  ncols = ncol(mat)
  colnames(mat)[(ncols-8):ncols] <- c('B.dp', 'W.dp', 'BxW.dp', 
                                      'B.Ap', 'W.Ap', 'BxW.Ap', 
                                      'B.Pr', 'W.Pr', 'BxW.Pr')
  
  if (model == 'THT'){
    for (sim in 1:nrow(mat)){ # for each set of params
      # generate Nr data sets
      
      # simulate data set
      tempDA = simTHT(nReps = Nreps, N_s = mat[sim, 'N_s'], N_t = mat[sim, 'N_t'], 
                      Pr_0 = mat[sim, 'Pr_0'], Pr_1 = mat[sim, 'Pr_1'], 
                      Pr_2 = mat[sim, 'Pr_2'], Pr_3 = mat[sim, 'Pr_3'],
                      Pr_SD = mat[sim, 'Pr_SD'], Br_0 = mat[sim, 'Br_0'], 
                      Br_1 = mat[sim, 'Br_1'], Br_2 = mat[sim, 'Br_2'], 
                      Br_3 = mat[sim, 'Br_3'], Br_SD = mat[sim, 'Br_SD'])
      # calculate measures
      tempDA <- calcMeasures(array = tempDA)
      # save simulated data set as an object
      if (saveDat){
        assign(x = paste0(matName, '_', sim), value = tempDA, envir=.GlobalEnv)
      }
      
      # extract ps
      tempPs = extractPs(tempDA)
      # calculate proportion sig
      propSig = apply(X = tempPs, MARGIN = 2, function(x) sum(x < 0.05)/nrow(tempPs))
      # add to d.f.
      names(propSig) <- NULL
      mat[sim, (ncols-8):ncols] <- propSig # add to last 9 columns
    }
    return(mat)
  } 
  if (model == 'SDT'){
    for (sim in 1:nrow(mat)){ # for each set of params
      # generate Nr data sets
      tempDA = simSDT(nReps = Nreps, N_s = mat[sim, 'N_s'], N_t = mat[sim, 'N_t'], 
                      d_0 = mat[sim, 'd_0'], d_1 = mat[sim, 'd_1'], 
                      d_2 = mat[sim, 'd_2'], d_3 = mat[sim, 'd_3'], 
                      d_SD = mat[sim, 'd_SD'], k_0 = mat[sim, 'k_0'], 
                      k_1 = mat[sim, 'k_1'], k_2 = mat[sim, 'k_2'], 
                      k_3 = mat[sim, 'k_3'], k_SD = mat[sim, 'k_SD'],
                      S = mat[sim, 'S'])
      # calculate measures
      tempDA <- calcMeasures(array = tempDA)
      
      # save simulated data set as an object
      if (saveDat){
        assign(x = paste0(matName, '_', sim), value = tempDA, envir=.GlobalEnv)
      }
      
      # extract ps
      tempPs = extractPs(tempDA)
      propSig = apply(X = tempPs, MARGIN = 2, function(x) sum(x < 0.05)/nrow(tempPs))
      # add to d.f.
      names(propSig) <- NULL
      mat[sim, (ncols-8):ncols] <- propSig
    }
    return(mat)
  }
}
