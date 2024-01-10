# ------ PARAMETER RECOVERY: HIERARCHICAL GROUP LEVEL MODEL

# setup
install.packages("pacman")
pacman::p_load(hesim, truncnorm, extraDistr, R2jags, parallel, ggpubr, ggplot2, tidyverse)

set.seed(1998)
setwd("/work/KlaraKrøyerFomsgaard#1926/DecMak_exam")

# defining a function for calculating the maximum of the posterior density
MPD <- function(x) {
  density(x)$x[which(density(x)$y==max(density(x)$y))]
}

#---set params
ntrials <- 15
ngroups <- 12
groupSize <- 3
ntokens <- 20
vals <- seq(0,ntokens,1)

# --------------------- Run full parameter recovery -------------------
niterations <- 100

# Sourcing in simulation function
source("CC_sim_group.r")

# --- True params
# mu
true_mu_alpha <- array(NA,c(niterations))
true_mu_rho <- array(NA,c(niterations))
true_mu_omega <- array(NA,c(niterations))


# sigma (SD for R) /lambda (precision for JAGS)
true_lambda_alpha <- array(NA,c(niterations))
true_lambda_rho <- array(NA,c(niterations))
true_lambda_omega <- array(NA,c(niterations))

# --- Inferred params

# mu 
infer_mu_alpha <- array(NA,c(niterations))
infer_mu_rho <- array(NA,c(niterations))
infer_mu_omega <- array(NA,c(niterations))


# sigma (SD for R) /lambda (precision for JAGS)
infer_lambda_alpha <- array(NA,c(niterations))
infer_lambda_rho <- array(NA,c(niterations))
infer_lambda_omega <- array(NA,c(niterations))

# Run iterations
for (i in 1:niterations){
  
  # Sample mu & sigma
  mu_alpha <- runif(1,0,20)
  mu_rho <-  runif(1,0.001,0.999)
  mu_omega <- runif(1,0.001,0.999)
  
  sigma_alpha <- runif(1,0,0.1)
  sigma_rho <- runif(1,0,0.1)
  sigma_omega <- runif(1,0,0.1)
  
  # Simulate data
  CC_sims <- CC_sim_hier(mu_alpha,
                           mu_rho,
                           mu_omega,
                           sigma_alpha,
                           sigma_rho,
                           sigma_omega,
                           ngroups,
                           groupSize,
                           ntrials)
  
  # Retrieve simulated data
  c <- CC_sims$c
  Ga <- CC_sims$Ga
  Gb <- CC_sims$Gb
  p <- CC_sims$p
  
  data_list <- list(
    ngroups = ngroups,
    ntrials = ntrials, 
    groupSize = groupSize,
    Ga = Ga,
    c = c)
  
  # -- Define model parameters for retrieval
  params <- c("mu_omega", "mu_rho", "mu_alpha","lambda_omega","lambda_rho","lambda_alpha")
  
  # Fit model
  samples <- jags.parallel(data = data_list,
                  inits=NULL,
                  parameters.to.save = params,
                  model.file ="CC_model_group.txt", 
                  n.chains = 3,
                  n.iter=40000, 
                  n.burnin=8000, 
                  n.thin=1,
                  n.cluster=3)
  
  # True parameter values
  ## Mu
  true_mu_alpha[i] <- mu_alpha
  true_mu_rho[i] <- mu_rho
  true_mu_omega[i] <- mu_omega
  
  ## Lambda
  true_lambda_alpha[i] <- sigma_alpha
  true_lambda_rho[i] <- sigma_rho
  true_lambda_omega[i] <- sigma_omega
  
  # Inferred parameter values
  ## Mu
  Y <- samples$BUGSoutput$sims.list
  infer_mu_alpha[i] <- MPD(Y$mu_alpha)
  infer_mu_rho[i] <- MPD(Y$mu_rho)
  infer_mu_omega[i] <- MPD(Y$mu_omega)
  
  ## Lambda
  infer_lambda_alpha[i] <- MPD(Y$lambda_alpha)
  infer_lambda_rho[i] <- MPD(Y$lambda_rho)
  infer_lambda_omega[i] <- MPD(Y$lambda_omega)
  
  print(i)
}
  
# ------- PLOT RECOVERY
source("pretty_recov_plot.R")

pl1 <- recov_plot(true_mu_alpha, infer_mu_alpha, c("true alpha", "recov alpha"), 'smoothed linear fit')
pl2 <- recov_plot(true_mu_rho, infer_mu_rho, c("true rho", "recov rho"), 'smoothed linear fit')
pl3 <- recov_plot(true_mu_omega, infer_mu_omega, c("true omega", "recov omega"), 'smoothed linear fit')
ggarrange(pl1, pl2, pl3)


pl1 <- recov_plot(true_lambda_alpha, infer_lambda_alpha, c("true alpha", "recov alpha"), 'smoothed linear fit')
pl2 <- recov_plot(true_lambda_rho, infer_lambda_rho, c("true rho", "recov rho"), 'smoothed linear fit')
pl3 <- recov_plot(true_lambda_omega, infer_lambda_omega, c("true omega", "recov omega"), 'smoothed linear fit')
ggarrange(pl1, pl2, pl3)


