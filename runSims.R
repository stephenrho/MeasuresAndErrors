# runSims.R
# Author: Stephen Rhodes (rhodessp at missouri.edu)
# License: GNU GPL v3.0

# Calls other scripts and runs simulations.
# Rhodes, Cowan, Parra, Logie "Interaction Effects on Common Measures of Sensitivity: Choice of Measure, Type I Error and Power"

TEST <- F # only runs 10 if TEST == T, 1000 otherwise

direc <- '~/github/MeasuresAndErrors/'
setwd(direc)

dir.create(path = 'results', showWarnings = F)
dir.create(path = 'results/tables', showWarnings = F)

source('modelFunctions.R')
source('simFunctions.R')

# for ordering columns
THTcols <- c("N_s", "N_t", "Pr_0", "Pr_1", "Pr_2", "Pr_3", "Pr_SD", "Br_0", "Br_1", "Br_2", "Br_3", "Br_SD")
SDTcols <- c("N_s", "N_t", "d_0", "d_1", "d_2", "d_3", "d_SD", "k_0", "k_1", "k_2", "k_3", "k_SD", "S")

## for saving sims
saveAndRemove <- function(simObj, saveRData = T){
  # this function creates a table of type-i/ii errors
  # and writes simulated data sets to an .RData file
  simName <- deparse(substitute(simObj))
  simNum <- sub('_.*', '', simName)
  
  write.table(x = simObj, paste0('results/tables/', simName))
  
  toSave = ls(pattern = simName, pos = ".GlobalEnv")
  if (saveRData){
    dir.create(paste0('results/', simNum), showWarnings = F)
    
    save(list = toSave, file = paste0('results/', simNum, '/', simName, '.RData'))
  }
  rm(list=toSave, pos = ".GlobalEnv")
}

#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
#### TYPE I ERROR SIMULATIONS
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####

## number of reps per parameter combination
if (TEST){
  Nr <- 10
} else {
  Nr <- 1000
}

### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ 
### Simulation 1: -----
# vary N_s, grand mean, size of effects, no variation in bias

### SIM 1 - THT ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
N_s <- c(12, 24, 48)
N_t <- 24 # per condition

B0_Pr <- seq(.4, .8, .1)
B1_Pr <- c(0, .025, .05, .1)
SD_Pr <- .1

# set up data frame of parameters
sim1_THT <- expand.grid(N_s = N_s, N_t = N_t, 
                       Pr_0 = B0_Pr, Pr_1 = B1_Pr, Pr_3 = 0, Pr_SD = SD_Pr, 
                       Br_0 = 0.5, Br_1 = 0, Br_2 = 0, Br_3 = 0, Br_SD = 0)

sim1_THT$Pr_2 <- sim1_THT$Pr_1 # same size effect for both...

# order columns
sim1_THT <- sim1_THT[THTcols]

sim1_THT <- runSims(mat = sim1_THT, model = 'THT', Nreps = Nr)

# save and then remove items
saveAndRemove(sim1_THT)

### SIM 1 - EV-SDT ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
B0_d <- seq(1.5, 3.5, .5)
B1_d <- c(0, .125, .25, .5)
SD_d <- .5

# set up data frame of parameters
sim1_EVSDT <- expand.grid(N_s = N_s, N_t = N_t, 
                         d_0 = B0_d, d_1 = B1_d, d_3 = 0, d_SD = SD_d, 
                         k_0 = 0, k_1 = 0, k_2 = 0, k_3 = 0, k_SD = 0, 
                         S = 1)

sim1_EVSDT$d_2 <- sim1_EVSDT$d_1

sim1_EVSDT <- sim1_EVSDT[SDTcols]

sim1_EVSDT <- runSims(mat = sim1_EVSDT, model = 'SDT', Nreps = Nr)

# save and then remove items
saveAndRemove(sim1_EVSDT)

### SIM 1 - UEV-SDT (s = .8) ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 

# set up data frame of parameters
sim1_UEVSDT_0.8 <- expand.grid(N_s = N_s, N_t = N_t, 
                          d_0 = B0_d, d_1 = B1_d, d_3 = 0, d_SD = SD_d, 
                          k_0 = 0, k_1 = 0, k_2 = 0, k_3 = 0, k_SD = 0, 
                          S = .8)

sim1_UEVSDT_0.8$d_2 <- sim1_UEVSDT_0.8$d_1

sim1_UEVSDT_0.8 <- sim1_UEVSDT_0.8[SDTcols]

sim1_UEVSDT_0.8 <- runSims(mat = sim1_UEVSDT_0.8, model = 'SDT', Nreps = Nr)

# save and then remove items
saveAndRemove(sim1_UEVSDT_0.8)

### SIM 1 - UEV-SDT (s = 1.2) ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 

# set up data frame of parameters
sim1_UEVSDT_1.2 <- expand.grid(N_s = N_s, N_t = N_t, 
                               d_0 = B0_d, d_1 = B1_d, d_3 = 0, d_SD = SD_d, 
                               k_0 = 0, k_1 = 0, k_2 = 0, k_3 = 0, k_SD = 0, 
                               S = 1.2)

sim1_UEVSDT_1.2$d_2 <- sim1_UEVSDT_1.2$d_1

sim1_UEVSDT_1.2 <- sim1_UEVSDT_1.2[SDTcols]

sim1_UEVSDT_1.2 <- runSims(mat = sim1_UEVSDT_1.2, model = 'SDT', Nreps = Nr)

# save and then remove items
saveAndRemove(sim1_UEVSDT_1.2)

### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ 
### Simulation 2: -----
# vary N_t rather than N_s

### SIM 2 - THT ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
N_s <- 24
N_t <- c(12, 24, 48) # per condition

B0_Pr <- seq(.4, .8, .1)
B1_Pr <- c(0, .025, .05, .1)
SD_Pr <- .1

sim2_THT <- expand.grid(N_s = N_s, N_t = N_t, 
                       Pr_0 = B0_Pr, Pr_1 = B1_Pr, Pr_3 = 0, Pr_SD = SD_Pr, 
                       Br_0 = 0.5, Br_1 = 0, Br_2 = 0, Br_3 = 0, Br_SD = 0)

sim2_THT$Pr_2 <- sim2_THT$Pr_1 # same size effect for both...
sim2_THT <- sim2_THT[THTcols]

sim2_THT <- runSims(mat = sim2_THT, model = 'THT', Nreps = Nr)

saveAndRemove(sim2_THT)

### SIM 2 - EV-SDT ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~
B0_d <- seq(1.5, 3.5, .5)
B1_d <- c(0, .125, .25, .5)
SD_d <- .5

sim2_EVSDT <- expand.grid(N_s = N_s, N_t = N_t, 
                         d_0 = B0_d, d_1 = B1_d, d_3 = 0, d_SD = SD_d, 
                         k_0 = 0, k_1 = 0, k_2 = 0, k_3 = 0, k_SD = 0, 
                         S = 1)

sim2_EVSDT$d_2 <- sim2_EVSDT$d_1
sim2_EVSDT <- sim2_EVSDT[SDTcols]

sim2_EVSDT <- runSims(mat = sim2_EVSDT, model = 'SDT', Nreps = Nr)

saveAndRemove(sim2_EVSDT)

### SIM 2 - UEV-SDT (s = 0.8) ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
sim2_UEVSDT_0.8 <- expand.grid(N_s = N_s, N_t = N_t, 
                          d_0 = B0_d, d_1 = B1_d, d_3 = 0, d_SD = SD_d, 
                          k_0 = 0, k_1 = 0, k_2 = 0, k_3 = 0, k_SD = 0, 
                          S = 0.8)

sim2_UEVSDT_0.8$d_2 <- sim2_UEVSDT_0.8$d_1
sim2_UEVSDT_0.8 <- sim2_UEVSDT_0.8[SDTcols]

sim2_UEVSDT_0.8 <- runSims(mat = sim2_UEVSDT_0.8, model = 'SDT', Nreps = Nr)

saveAndRemove(sim2_UEVSDT_0.8)

### SIM 2 - UEV-SDT (s = 1.2) ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
sim2_UEVSDT_1.2 <- expand.grid(N_s = N_s, N_t = N_t, 
                               d_0 = B0_d, d_1 = B1_d, d_3 = 0, d_SD = SD_d, 
                               k_0 = 0, k_1 = 0, k_2 = 0, k_3 = 0, k_SD = 0, 
                               S = 1.2)

sim2_UEVSDT_1.2$d_2 <- sim2_UEVSDT_1.2$d_1
sim2_UEVSDT_1.2 <- sim2_UEVSDT_1.2[SDTcols]

sim2_UEVSDT_1.2 <- runSims(mat = sim2_UEVSDT_1.2, model = 'SDT', Nreps = Nr)

saveAndRemove(sim2_UEVSDT_1.2)

### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ 
### Simulation 3: -----
# mix size of effects

### SIM 3 - THT ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 

N_s <- 24 # fix both N_s and N_t to 24
N_t <- 24

B0_Pr <- seq(.4, .8, .1)
B1_Pr <- c(0, .025, .05, .1)
SD_Pr <- .1

sim3_THT <- expand.grid(N_s = N_s, N_t = N_t, 
                       Pr_0 = B0_Pr, Pr_1 = B1_Pr, Pr_2 = B1_Pr, Pr_3 = 0, Pr_SD = SD_Pr, 
                       Br_0 = 0.5, Br_1 = 0, Br_2 = 0, Br_3 = 0, Br_SD = 0)

sim3_THT <- runSims(mat = sim3_THT, model = 'THT', Nreps = Nr)

saveAndRemove(sim3_THT)

### SIM 3 - EV-SDT ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
B0_d <- seq(1.5, 3.5, .5)
B1_d <- c(0, .125, .25, .5)
SD_d <- .5

sim3_EVSDT <- expand.grid(N_s = N_s, N_t = N_t, 
                         d_0 = B0_d, d_1 = B1_d, d_2 = B1_d, d_3 = 0, d_SD = SD_d, 
                         k_0 = 0, k_1 = 0, k_2 = 0, k_3 = 0, k_SD = 0, 
                         S = 1)

sim3_EVSDT <- runSims(mat = sim3_EVSDT, model = 'SDT', Nreps = Nr)

saveAndRemove(sim3_EVSDT)

### SIM 3 - UEV-SDT (s = 0.8) ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
sim3_UEVSDT_0.8 <- expand.grid(N_s = N_s, N_t = N_t, 
                          d_0 = B0_d, d_1 = B1_d, d_2 = B1_d, d_3 = 0, d_SD = SD_d, 
                          k_0 = 0, k_1 = 0, k_2 = 0, k_3 = 0, k_SD = 0, 
                          S = 0.8)

sim3_UEVSDT_0.8 <- runSims(mat = sim3_UEVSDT_0.8, model = 'SDT', Nreps = Nr)

saveAndRemove(sim3_UEVSDT_0.8)

### SIM 3 - UEV-SDT (s = 1.2) ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
sim3_UEVSDT_1.2 <- expand.grid(N_s = N_s, N_t = N_t, 
                               d_0 = B0_d, d_1 = B1_d, d_2 = B1_d, d_3 = 0, d_SD = SD_d, 
                               k_0 = 0, k_1 = 0, k_2 = 0, k_3 = 0, k_SD = 0, 
                               S = 1.2)

sim3_UEVSDT_1.2 <- runSims(mat = sim3_UEVSDT_1.2, model = 'SDT', Nreps = Nr)

saveAndRemove(sim3_UEVSDT_1.2)

### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ 
### Simulation 4: -----
# add s variation in bias and vary grand mean

### SIM 4 - THT ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
B0_Pr <- seq(.4, .8, .1)
B1_Pr <- c(0, .025, .05, .1)
SD_Pr <- .1

B0_Br <- seq(.3, .7, .1)
B1_Br <- 0
SD_Br <- .1

sim4_THT <- expand.grid(N_s = N_s, N_t = N_t, 
                       Pr_0 = B0_Pr, Pr_1 = B1_Pr, Pr_3 = 0, Pr_SD = SD_Pr, 
                       Br_0 = B0_Br, Br_1 = 0, Br_2 = 0, Br_3 = 0, Br_SD = SD_Br)

sim4_THT$Pr_2 <- sim4_THT$Pr_1 # same size effect for both...
sim4_THT <- sim4_THT[THTcols]

sim4_THT <- runSims(mat = sim4_THT, model = 'THT', Nreps = Nr)

saveAndRemove(sim4_THT)

### SIM 4 - EV-SDT ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
B0_d <- seq(1.5, 3.5, .5)
B1_d <- c(0, .125, .25, .5)
SD_d <- .5

B0_k <- seq(-1, 1, .5)
B1_k <- 0
SD_k <- .5

sim4_EVSDT <- expand.grid(N_s = N_s, N_t = N_t, 
                         d_0 = B0_d, d_1 = B1_d, d_3 = 0, d_SD = SD_d, 
                         k_0 = B0_k, k_1 = 0, k_2 = 0, k_3 = 0, k_SD = SD_k, 
                         S = 1)

sim4_EVSDT$d_2 <- sim4_EVSDT$d_1
sim4_EVSDT <- sim4_EVSDT[SDTcols]

sim4_EVSDT <- runSims(mat = sim4_EVSDT, model = 'SDT', Nreps = Nr)

saveAndRemove(sim4_EVSDT)

### SIM 4 - UEVSDT (s = 0.8) ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
sim4_UEVSDT_0.8 <- expand.grid(N_s = N_s, N_t = N_t, 
                          d_0 = B0_d, d_1 = B1_d, d_3 = 0, d_SD = SD_d, 
                          k_0 = B0_k, k_1 = 0, k_2 = 0, k_3 = 0, k_SD = SD_k, 
                          S = 0.8)

sim4_UEVSDT_0.8$d_2 <- sim4_UEVSDT_0.8$d_1
sim4_UEVSDT_0.8 <- sim4_UEVSDT_0.8[SDTcols]

sim4_UEVSDT_0.8 <- runSims(mat = sim4_UEVSDT_0.8, model = 'SDT', Nreps = Nr)

saveAndRemove(sim4_UEVSDT_0.8)

### SIM 4 - UEVSDT (s = 1.2) ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
sim4_UEVSDT_1.2 <- expand.grid(N_s = N_s, N_t = N_t, 
                               d_0 = B0_d, d_1 = B1_d, d_3 = 0, d_SD = SD_d, 
                               k_0 = B0_k, k_1 = 0, k_2 = 0, k_3 = 0, k_SD = SD_k, 
                               S = 1.2)

sim4_UEVSDT_1.2$d_2 <- sim4_UEVSDT_1.2$d_1
sim4_UEVSDT_1.2 <- sim4_UEVSDT_1.2[SDTcols]

sim4_UEVSDT_1.2 <- runSims(mat = sim4_UEVSDT_1.2, model = 'SDT', Nreps = Nr)

saveAndRemove(sim4_UEVSDT_1.2)

### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ 
### Simulation 5: -----
# vary bias by condition. first by between var then by both 

### SIM 5 - THT ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
B0_Pr <- seq(.4, .8, .1)
B1_Pr <- .1 # fix sensitivity effect
SD_Pr <- .1

B0_Br <- seq(.3, .7, .1)
B1_Br <- c(.025, .05, .1) # vary bias effect
SD_Br <- .1

sim5_THT <- expand.grid(N_s = N_s, N_t = N_t, 
                       Pr_0 = B0_Pr, Pr_1 = B1_Pr, Pr_2 = B1_Pr, Pr_3 = 0, Pr_SD = SD_Pr, 
                       Br_0 = B0_Br, Br_1 = B1_Br, Br_2 = 0, Br_3 = 0, Br_SD = SD_Br)

sim5_THT <- runSims(mat = sim5_THT, model = 'THT', Nreps = Nr)

saveAndRemove(sim5_THT)

### SIM 5 - EV-SDT ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
B0_d <- seq(1.5, 3.5, .5)
B1_d <- .25
SD_d <- .5

B0_k <- seq(-1, 1, .5)
B1_k <- c(.125, .25, .5)
SD_k <- .5

sim5_EVSDT <- expand.grid(N_s = N_s, N_t = N_t, 
                         d_0 = B0_d, d_1 = B1_d, d_2 = B1_d, d_3 = 0, d_SD = SD_d, 
                         k_0 = B0_k, k_1 = B1_k, k_2 = 0, k_3 = 0, k_SD = SD_k, 
                         S = 1)

sim5_EVSDT <- runSims(mat = sim5_EVSDT, model = 'SDT', Nreps = Nr)

saveAndRemove(sim5_EVSDT)

### SIM 5 - UEVSDT (s = 0.8) ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
sim5_UEVSDT_0.8 <- expand.grid(N_s = N_s, N_t = N_t, 
                          d_0 = B0_d, d_1 = B1_d, d_2 = B1_d, d_3 = 0, d_SD = SD_d, 
                          k_0 = B0_k, k_1 = B1_k, k_2 = 0, k_3 = 0, k_SD = SD_k, 
                          S = 0.8)

sim5_UEVSDT_0.8 <- runSims(mat = sim5_UEVSDT_0.8, model = 'SDT', Nreps = Nr)

saveAndRemove(sim5_UEVSDT_0.8)

### SIM 5 - UEVSDT (s = 1.2) ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
sim5_UEVSDT_1.2 <- expand.grid(N_s = N_s, N_t = N_t, 
                               d_0 = B0_d, d_1 = B1_d, d_2 = B1_d, d_3 = 0, d_SD = SD_d, 
                               k_0 = B0_k, k_1 = B1_k, k_2 = 0, k_3 = 0, k_SD = SD_k, 
                               S = 1.2)

sim5_UEVSDT_1.2 <- runSims(mat = sim5_UEVSDT_1.2, model = 'SDT', Nreps = Nr)

saveAndRemove(sim5_UEVSDT_1.2)

### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ 
### Simulation 6: -----
# fix sensitivity mean and main effect and vary bias mean and main effects...

### SIM 6 - THT ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
B0_Pr = .6
B1_Pr = .1
SD_Pr <- .1

B0_Br = seq(.3, .7, .1)
B1_Br = c(0, .025, .05, .1)
SD_Br <- .1

sim6_THT <- expand.grid(N_s = N_s, N_t = N_t, 
                        Pr_0 = B0_Pr, Pr_1 = B1_Pr, Pr_2 = B1_Pr, Pr_3 = 0, Pr_SD = SD_Pr, 
                        Br_0 = B0_Br, Br_1 = B1_Br, Br_2 = B1_Br, Br_3 = 0, Br_SD = SD_Br)

sim6_THT <- runSims(mat = sim6_THT, model = 'THT', Nreps = Nr)

saveAndRemove(sim6_THT)

### SIM 6 - EV-SDT ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
B0_d = 2
B1_d <- .25
SD_d <- .5

B0_k <- seq(-1, 1, .5)
B1_k = c(0, .125, .25, .5)
SD_k <- .5

sim6_EVSDT <- expand.grid(N_s = N_s, N_t = N_t, 
                          d_0 = B0_d, d_1 = B1_d, d_2 = B1_d, d_3 = 0, d_SD = SD_d, 
                          k_0 = B0_k, k_1 = B1_k, k_2 = B1_k, k_3 = 0, k_SD = SD_k, 
                          S = 1)

sim6_EVSDT <- runSims(mat = sim6_EVSDT, model = 'SDT', Nreps = Nr)

saveAndRemove(sim6_EVSDT)

### SIM 6 - UEV-SDT (s = 0.8) ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
sim6_UEVSDT_0.8 <- expand.grid(N_s = N_s, N_t = N_t, 
                          d_0 = B0_d, d_1 = B1_d, d_2 = B1_d, d_3 = 0, d_SD = SD_d, 
                          k_0 = B0_k, k_1 = B1_k, k_2 = B1_k, k_3 = 0, k_SD = SD_k, 
                          S = 0.8)

sim6_UEVSDT_0.8 <- runSims(mat = sim6_UEVSDT_0.8, model = 'SDT', Nreps = Nr)

saveAndRemove(sim6_UEVSDT_0.8)

### SIM 6 - UEV-SDT (s = 1.2) ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
sim6_UEVSDT_1.2 <- expand.grid(N_s = N_s, N_t = N_t, 
                               d_0 = B0_d, d_1 = B1_d, d_2 = B1_d, d_3 = 0, d_SD = SD_d, 
                               k_0 = B0_k, k_1 = B1_k, k_2 = B1_k, k_3 = 0, k_SD = SD_k, 
                               S = 1.2)

sim6_UEVSDT_1.2 <- runSims(mat = sim6_UEVSDT_1.2, model = 'SDT', Nreps = Nr)

saveAndRemove(sim6_UEVSDT_1.2)

### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ 
### Simulation 7: -----
# fix sensitivity mean WITH NO MAIN EFFECT and vary bias mean and main effects...

### SIM 7 - THT ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
B0_Pr = .6
B1_Pr = 0 # NO MAIN EFFECTS
SD_Pr <- .1

B0_Br = seq(.3, .7, .1)
B1_Br = c(0, .025, .05, .1)
SD_Br <- .1

sim7_THT <- expand.grid(N_s = N_s, N_t = N_t, 
                        Pr_0 = B0_Pr, Pr_1 = B1_Pr, Pr_2 = B1_Pr, Pr_3 = 0, Pr_SD = SD_Pr, 
                        Br_0 = B0_Br, Br_1 = B1_Br, Br_2 = B1_Br, Br_3 = 0, Br_SD = SD_Br)

sim7_THT <- runSims(mat = sim7_THT, model = 'THT', Nreps = Nr)

saveAndRemove(sim7_THT)

### SIM 7 - EV-SDT ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
B0_d = 2
B1_d = 0 # NO MAIN EFFECT
SD_d <- .5

B0_k <- seq(-1, 1, .5)
B1_k = c(0, .125, .25, .5)
SD_k <- .5

sim7_EVSDT <- expand.grid(N_s = N_s, N_t = N_t, 
                          d_0 = B0_d, d_1 = B1_d, d_2 = B1_d, d_3 = 0, d_SD = SD_d, 
                          k_0 = B0_k, k_1 = B1_k, k_2 = B1_k, k_3 = 0, k_SD = SD_k, 
                          S = 1)

sim7_EVSDT <- runSims(mat = sim7_EVSDT, model = 'SDT', Nreps = Nr)

saveAndRemove(sim7_EVSDT)

### SIM 7 - UEV-SDT (s = 0.8) ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
sim7_UEVSDT_0.8 <- expand.grid(N_s = N_s, N_t = N_t, 
                          d_0 = B0_d, d_1 = B1_d, d_2 = B1_d, d_3 = 0, d_SD = SD_d, 
                          k_0 = B0_k, k_1 = B1_k, k_2 = B1_k, k_3 = 0, k_SD = SD_k, 
                          S = 0.8)

sim7_UEVSDT_0.8 <- runSims(mat = sim7_UEVSDT_0.8, model = 'SDT', Nreps = Nr)

saveAndRemove(sim7_UEVSDT_0.8)

### SIM 7 - UEV-SDT (s = 1.2) ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
sim7_UEVSDT_1.2 <- expand.grid(N_s = N_s, N_t = N_t, 
                               d_0 = B0_d, d_1 = B1_d, d_2 = B1_d, d_3 = 0, d_SD = SD_d, 
                               k_0 = B0_k, k_1 = B1_k, k_2 = B1_k, k_3 = 0, k_SD = SD_k, 
                               S = 1.2)

sim7_UEVSDT_1.2 <- runSims(mat = sim7_UEVSDT_1.2, model = 'SDT', Nreps = Nr)

saveAndRemove(sim7_UEVSDT_1.2)

#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
#### POWER SIMULATIONS
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####

### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ 
### Simulation 8: -----

## Vary number of participants (or trials) and magnitude of interaction effect
## having both B_1 and B_2 positive means B_3 > 0 = over-additive whereas 
## B_3 < 0 = under-additive

### SIM 8 - THT ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
N_s <- c(12, 24, 48)
N_t <- 24 # per condition

B0_Pr <- seq(.4, .8, .1)
B1_Pr <- c(.025, .05, .1)
B3_Pr <- c(-.05, -.025, .025, .05)
SD_Pr <- .1

sim8_THT <- expand.grid(N_s = N_s, N_t = N_t, 
                        Pr_0 = B0_Pr, Pr_1 = B1_Pr, Pr_3 = B3_Pr, Pr_SD = SD_Pr, 
                        Br_0 = 0.5, Br_1 = 0, Br_2 = 0, Br_3 = 0, Br_SD = 0)

sim8_THT$Pr_2 <- sim8_THT$Pr_1
sim8_THT <- sim8_THT[THTcols]

sim8_THT <- runSims(mat = sim8_THT, model = 'THT', Nreps = Nr, saveDat = F)

saveAndRemove(sim8_THT, saveRData = F)

### SIM 8 - EV-SDT ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
B0_d <- seq(1.5, 3.5, .5)
B1_d <- c(.125, .25, .5)
B3_d <- c(-.25, -.125, .125, .25)
SD_d <- .5

sim8_EVSDT <- expand.grid(N_s = N_s, N_t = N_t, 
                          d_0 = B0_d, d_1 = B1_d, d_3 = B3_d, d_SD = SD_d, 
                          k_0 = 0, k_1 = 0, k_2 = 0, k_3 = 0, k_SD = 0, 
                          S = 1)

sim8_EVSDT$d_2 <- sim8_EVSDT$d_1
sim8_EVSDT <- sim8_EVSDT[SDTcols]

sim8_EVSDT <- runSims(mat = sim8_EVSDT, model = 'SDT', Nreps = Nr, saveDat = F)

saveAndRemove(sim8_EVSDT, saveRData = F)

### SIM 8 - UEV-SDT (s = 0.8) ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 

sim8_UEVSDT_0.8 <- expand.grid(N_s = N_s, N_t = N_t, 
                               d_0 = B0_d, d_1 = B1_d, d_3 = B3_d, d_SD = SD_d, 
                               k_0 = 0, k_1 = 0, k_2 = 0, k_3 = 0, k_SD = 0, 
                               S = .8)

sim8_UEVSDT_0.8$d_2 <- sim8_UEVSDT_0.8$d_1
sim8_UEVSDT_0.8 <- sim8_UEVSDT_0.8[SDTcols]

sim8_UEVSDT_0.8 <- runSims(mat = sim8_UEVSDT_0.8, model = 'SDT', Nreps = Nr, saveDat = F)

saveAndRemove(sim8_UEVSDT_0.8, saveRData = F)

### SIM 8 - UEV-SDT (s = 1.2) ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 

sim8_UEVSDT_1.2 <- expand.grid(N_s = N_s, N_t = N_t, 
                               d_0 = B0_d, d_1 = B1_d, d_3 = B3_d, d_SD = SD_d, 
                               k_0 = 0, k_1 = 0, k_2 = 0, k_3 = 0, k_SD = 0, 
                               S = 1.2)

sim8_UEVSDT_1.2$d_2 <- sim8_UEVSDT_1.2$d_1
sim8_UEVSDT_1.2 <- sim8_UEVSDT_1.2[SDTcols]

sim8_UEVSDT_1.2 <- runSims(mat = sim8_UEVSDT_1.2, model = 'SDT', Nreps = Nr, saveDat = F)

saveAndRemove(sim8_UEVSDT_1.2, saveRData = F)

### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ 
### Simulation 9: -----

### SIM 9 - THT ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
N_t <- c(12, 24, 48)
N_s <- 24 # per condition

B0_Pr <- seq(.4, .8, .1)
B1_Pr <- c(.025, .05, .1)
B3_Pr <- c(-.05, -.025, .025, .05)
SD_Pr <- .1

sim9_THT <- expand.grid(N_s = N_s, N_t = N_t, 
                        Pr_0 = B0_Pr, Pr_1 = B1_Pr, Pr_3 = B3_Pr, Pr_SD = SD_Pr, 
                        Br_0 = 0.5, Br_1 = 0, Br_2 = 0, Br_3 = 0, Br_SD = 0)

sim9_THT$Pr_2 <- sim9_THT$Pr_1
sim9_THT <- sim9_THT[THTcols]

sim9_THT <- runSims(mat = sim9_THT, model = 'THT', Nreps = Nr, saveDat = F)

saveAndRemove(sim9_THT, saveRData = F)

### SIM 9 - EV-SDT ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
B0_d <- seq(1.5, 3.5, .5)
B1_d <- c(.125, .25, .5)
B3_d <- c(-.25, -.125, .125, .25)
SD_d <- .5

sim9_EVSDT <- expand.grid(N_s = N_s, N_t = N_t, 
                          d_0 = B0_d, d_1 = B1_d, d_3 = B3_d, d_SD = SD_d, 
                          k_0 = 0, k_1 = 0, k_2 = 0, k_3 = 0, k_SD = 0, 
                          S = 1)

sim9_EVSDT$d_2 <- sim9_EVSDT$d_1
sim9_EVSDT <- sim9_EVSDT[SDTcols]

sim9_EVSDT <- runSims(mat = sim9_EVSDT, model = 'SDT', Nreps = Nr, saveDat = F)

saveAndRemove(sim9_EVSDT, saveRData = F)

### SIM 9 - UEV-SDT (s = 0.8) ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 

sim9_UEVSDT_0.8 <- expand.grid(N_s = N_s, N_t = N_t, 
                               d_0 = B0_d, d_1 = B1_d, d_3 = B3_d, d_SD = SD_d, 
                               k_0 = 0, k_1 = 0, k_2 = 0, k_3 = 0, k_SD = 0, 
                               S = .8)

sim9_UEVSDT_0.8$d_2 <- sim9_UEVSDT_0.8$d_1
sim9_UEVSDT_0.8 <- sim9_UEVSDT_0.8[SDTcols]

sim9_UEVSDT_0.8 <- runSims(mat = sim9_UEVSDT_0.8, model = 'SDT', Nreps = Nr, saveDat = F)

saveAndRemove(sim9_UEVSDT_0.8, saveRData = F)

### SIM 9 - UEV-SDT (s = 1.2) ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 

sim9_UEVSDT_1.2 <- expand.grid(N_s = N_s, N_t = N_t, 
                               d_0 = B0_d, d_1 = B1_d, d_3 = B3_d, d_SD = SD_d, 
                               k_0 = 0, k_1 = 0, k_2 = 0, k_3 = 0, k_SD = 0, 
                               S = 1.2)

sim9_UEVSDT_1.2$d_2 <- sim9_UEVSDT_1.2$d_1
sim9_UEVSDT_1.2 <- sim9_UEVSDT_1.2[SDTcols]

sim9_UEVSDT_1.2 <- runSims(mat = sim9_UEVSDT_1.2, model = 'SDT', Nreps = Nr, saveDat = F)

saveAndRemove(sim9_UEVSDT_1.2, saveRData = F)

### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ 
### Simulation 10: -----
# add s variation in bias and vary grand mean
# fix main effects to largest size

N_t <- 24
N_s <- 24

B0_Pr <- seq(.4, .8, .1)
B1_Pr <- c(.025, .05, .1)
B3_Pr <- c(-.05, -.025, .025, .05)
SD_Pr <- .1

B0_Br <- seq(.3, .7, .1)
B1_Br <- 0
SD_Br <- .1

sim10_THT <- expand.grid(N_s = N_s, N_t = N_t, 
                        Pr_0 = B0_Pr, Pr_1 = B1_Pr, Pr_3 = B3_Pr, Pr_SD = SD_Pr, 
                        Br_0 = B0_Br, Br_1 = B1_Br, Br_2 = B1_Br, Br_3 = 0, Br_SD = SD_Br)

sim10_THT$Pr_2 <- sim10_THT$Pr_1 # same size effect for both...
sim10_THT <- sim10_THT[THTcols]

sim10_THT <- runSims(mat = sim10_THT, model = 'THT', Nreps = Nr, saveDat = F)

saveAndRemove(sim10_THT, saveRData = F)

### SIM 10 - EV-SDT ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
B0_d <- seq(1.5, 3.5, .5)
B1_d <- c(.125, .25, .5)
B3_d <- c(-.25, -.125, .125, .25)
SD_d <- .5

B0_k <- seq(-1, 1, .5)
B1_k <- 0
SD_k <- .5

sim10_EVSDT <- expand.grid(N_s = N_s, N_t = N_t, 
                          d_0 = B0_d, d_1 = B1_d, d_3 = B3_d, d_SD = SD_d, 
                          k_0 = B0_k, k_1 = B1_k, k_2 = B1_k, k_3 = 0, k_SD = SD_k, 
                          S = 1)

sim10_EVSDT$d_2 <- sim10_EVSDT$d_1
sim10_EVSDT <- sim10_EVSDT[SDTcols]

sim10_EVSDT <- runSims(mat = sim10_EVSDT, model = 'SDT', Nreps = Nr, saveDat = F)

saveAndRemove(sim10_EVSDT, saveRData = F)

### SIM 10 - UEV-SDT (s = .8) ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 

sim10_UEVSDT_0.8 <- expand.grid(N_s = N_s, N_t = N_t, 
                          d_0 = B0_d, d_1 = B1_d, d_3 = B3_d, d_SD = SD_d, 
                          k_0 = B0_k, k_1 = B1_k, k_2 = B1_k, k_3 = 0, k_SD = SD_k, 
                          S = .8)

sim10_UEVSDT_0.8$d_2 <- sim10_UEVSDT_0.8$d_1
sim10_UEVSDT_0.8 <- sim10_UEVSDT_0.8[SDTcols]

sim10_UEVSDT_0.8 <- runSims(mat = sim10_UEVSDT_0.8, model = 'SDT', Nreps = Nr, saveDat = F)

saveAndRemove(sim10_UEVSDT_0.8, saveRData = F)

### SIM 10 - UEV-SDT (s = 1.2) ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 

sim10_UEVSDT_1.2 <- expand.grid(N_s = N_s, N_t = N_t, 
                                d_0 = B0_d, d_1 = B1_d, d_3 = B3_d, d_SD = SD_d, 
                                k_0 = B0_k, k_1 = B1_k, k_2 = B1_k, k_3 = 0, k_SD = SD_k, 
                                S = 1.2)

sim10_UEVSDT_1.2$d_2 <- sim10_UEVSDT_1.2$d_1
sim10_UEVSDT_1.2 <- sim10_UEVSDT_1.2[SDTcols]

sim10_UEVSDT_1.2 <- runSims(mat = sim10_UEVSDT_1.2, model = 'SDT', Nreps = Nr, saveDat = F)

saveAndRemove(sim10_UEVSDT_1.2, saveRData = F)

### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ 
### Simulation 11: -----
# fix sensitivity grand mean, vary overall bias and main effects on bias

B0_Pr = .6
B1_Pr <- c(.025, .05, .1)
B3_Pr <- c(-.05, -.025, .025, .05)
SD_Pr <- .1

B0_Br <- seq(.3, .7, .1)
B1_Br = c(0, .025, .05, .1)
SD_Br <- .1

sim11_THT <- expand.grid(N_s = N_s, N_t = N_t, 
                         Pr_0 = B0_Pr, Pr_1 = B1_Pr, Pr_3 = B3_Pr, Pr_SD = SD_Pr, 
                         Br_0 = B0_Br, Br_1 = B1_Br, Br_3 = 0, Br_SD = SD_Br)

sim11_THT$Pr_2 <- sim11_THT$Pr_1 # same size effect for both...
sim11_THT$Br_2 <- sim11_THT$Br_1
sim11_THT <- sim11_THT[THTcols]

sim11_THT <- runSims(mat = sim11_THT, model = 'THT', Nreps = Nr, saveDat = F)

saveAndRemove(sim11_THT, saveRData = F)

### SIM 11 - EV-SDT ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 

B0_d = 2
B1_d <- c(.125, .25, .5)
B3_d <- c(-.25, -.125, .125, .25)
SD_d <- .5

B0_k <- seq(-1, 1, .5)
B1_k <- c(0, .125, .25, .5)
SD_k <- .5

sim11_EVSDT <- expand.grid(N_s = N_s, N_t = N_t, 
                           d_0 = B0_d, d_1 = B1_d, d_3 = B3_d, d_SD = SD_d, 
                           k_0 = B0_k, k_1 = B1_k, k_3 = 0, k_SD = SD_k, 
                           S = 1)

sim11_EVSDT$d_2 <- sim11_EVSDT$d_1
sim11_EVSDT$k_2 <- sim11_EVSDT$k_1
sim11_EVSDT <- sim11_EVSDT[SDTcols]

sim11_EVSDT <- runSims(mat = sim11_EVSDT, model = 'SDT', Nreps = Nr, saveDat = F)

saveAndRemove(sim11_EVSDT, saveRData = F)

### SIM 11 - UEV-SDT (s = 0.8) ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
sim11_UEVSDT_0.8 <- expand.grid(N_s = N_s, N_t = N_t, 
                           d_0 = B0_d, d_1 = B1_d, d_3 = B3_d, d_SD = SD_d, 
                           k_0 = B0_k, k_1 = B1_k, k_3 = 0, k_SD = SD_k, 
                           S = 0.8)

sim11_UEVSDT_0.8$d_2 <- sim11_UEVSDT_0.8$d_1
sim11_UEVSDT_0.8$k_2 <- sim11_UEVSDT_0.8$k_1
sim11_UEVSDT_0.8 <- sim11_UEVSDT_0.8[SDTcols]

sim11_UEVSDT_0.8 <- runSims(mat = sim11_UEVSDT_0.8, model = 'SDT', Nreps = Nr, saveDat = F)

saveAndRemove(sim11_UEVSDT_0.8, saveRData = F)

### SIM 11 - UEV-SDT (s = 1.2) ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
sim11_UEVSDT_1.2 <- expand.grid(N_s = N_s, N_t = N_t, 
                                d_0 = B0_d, d_1 = B1_d, d_3 = B3_d, d_SD = SD_d, 
                                k_0 = B0_k, k_1 = B1_k, k_3 = 0, k_SD = SD_k, 
                                S = 1.2)

sim11_UEVSDT_1.2$d_2 <- sim11_UEVSDT_1.2$d_1
sim11_UEVSDT_1.2$k_2 <- sim11_UEVSDT_1.2$k_1
sim11_UEVSDT_1.2 <- sim11_UEVSDT_1.2[SDTcols]

sim11_UEVSDT_1.2 <- runSims(mat = sim11_UEVSDT_1.2, model = 'SDT', Nreps = Nr, saveDat = F)

saveAndRemove(sim11_UEVSDT_1.2, saveRData = F)

### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ ### $$$ 
### Simulation 12: -----
# fix sensitivity grand mean, vary overall bias and main effects on bias
# with no sensitivity main effects

N_t <- 24
N_s <- 24

B0_Pr = .6
B1_Pr <- 0
B3_Pr <- c(-.05, -.025, .025, .05)
SD_Pr <- .1

B0_Br <- seq(.3, .7, .1)
B1_Br = c(0, .025, .05, .1)
SD_Br <- .1

sim12_THT <- expand.grid(N_s = N_s, N_t = N_t, 
                         Pr_0 = B0_Pr, Pr_1 = B1_Pr, Pr_2 = B1_Pr, Pr_3 = B3_Pr, Pr_SD = SD_Pr, 
                         Br_0 = B0_Br, Br_1 = B1_Br, Br_3 = 0, Br_SD = SD_Br)

sim12_THT$Br_2 <- sim12_THT$Br_1
sim12_THT <- sim12_THT[THTcols]

sim12_THT <- runSims(mat = sim12_THT, model = 'THT', Nreps = Nr, saveDat = F)

saveAndRemove(sim12_THT, saveRData = F)

### SIM 12 - EV-SDT ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 

B0_d = 2
B1_d <- 0
B3_d <- c(-.25, -.125, .125, .25)
SD_d <- .5

B0_k <- seq(-1, 1, .5)
B1_k <- c(0, .125, .25, .5)
SD_k <- .5

sim12_EVSDT <- expand.grid(N_s = N_s, N_t = N_t, 
                           d_0 = B0_d, d_1 = B1_d, d_2 = B1_d, d_3 = B3_d, d_SD = SD_d, 
                           k_0 = B0_k, k_1 = B1_k, k_3 = 0, k_SD = SD_k, 
                           S = 1)

sim12_EVSDT$k_2 <- sim12_EVSDT$k_1
sim12_EVSDT <- sim12_EVSDT[SDTcols]

sim12_EVSDT <- runSims(mat = sim12_EVSDT, model = 'SDT', Nreps = Nr, saveDat = F)

saveAndRemove(sim12_EVSDT, saveRData = F)

### SIM 12 - UEV-SDT (s = 0.8) ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
sim12_UEVSDT_0.8 <- expand.grid(N_s = N_s, N_t = N_t, 
                                d_0 = B0_d, d_1 = B1_d, d_2 = B1_d, d_3 = B3_d, d_SD = SD_d, 
                                k_0 = B0_k, k_1 = B1_k, k_3 = 0, k_SD = SD_k, 
                                S = 0.8)

sim12_UEVSDT_0.8$k_2 <- sim12_UEVSDT_0.8$k_1
sim12_UEVSDT_0.8 <- sim12_UEVSDT_0.8[SDTcols]

sim12_UEVSDT_0.8 <- runSims(mat = sim12_UEVSDT_0.8, model = 'SDT', Nreps = Nr, saveDat = F)

saveAndRemove(sim12_UEVSDT_0.8, saveRData = F)

### SIM 12 - UEV-SDT (s = 1.2) ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
sim12_UEVSDT_1.2 <- expand.grid(N_s = N_s, N_t = N_t, 
                                d_0 = B0_d, d_1 = B1_d, d_2 = B1_d, d_3 = B3_d, d_SD = SD_d, 
                                k_0 = B0_k, k_1 = B1_k, k_3 = 0, k_SD = SD_k, 
                                S = 1.2)

sim12_UEVSDT_1.2$k_2 <- sim12_UEVSDT_1.2$k_1
sim12_UEVSDT_1.2 <- sim12_UEVSDT_1.2[SDTcols]

sim12_UEVSDT_1.2 <- runSims(mat = sim12_UEVSDT_1.2, model = 'SDT', Nreps = Nr, saveDat = F)

saveAndRemove(sim12_UEVSDT_1.2, saveRData = F)

