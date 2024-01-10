# ------- PARAMETER RECOVERY: MODEL VALIDATION -------


# Setup
install.packages("pacman")
pacman::p_load(extraDistr, parallel, ggpubr, ggplot2, tidyverse, hesim)
install.packages('R2jags')
library(R2jags)

set.seed(1998) # best year
setwd("/work/KlaraKrøyerFomsgaard#1926/DecMak_exam")


# defining a function for calculating the maximum of the posterior density
MPD <- function(x) {
  density(x)$x[which(density(x)$y==max(density(x)$y))]
}


#-------Create simulated data ---------
#---set params
ntrials <- 15
ngroups <- 960 # We increase data amount by simulating more groups 
groupSize <- 3
ntokens <- 20
vals <- seq(0,ntokens,1)

# -- Defining 
alpha <- matrix(runif(ngroups*groupSize,0,20), nrow=groupSize, byrow=TRUE) 
rho <- matrix(runif(ngroups*groupSize,0,1), nrow=groupSize, byrow=TRUE) 
omega <- matrix(runif(ngroups*groupSize,0,1), nrow=groupSize, byrow=TRUE)

# Sourcing in simulation function
source("CC_sim_individual.R")

# Simulate
CC_sims <- CC_sim_individual(alpha,
                         rho,
                         omega,
                         ngroups,
                         groupSize,
                         ntrials)

# Save simulated data 
c <- CC_sims$c
Ga <- CC_sims$Ga
Gb <- CC_sims$Gb
p <- CC_sims$p


par(mfrow=c(2,2))

plot(CC_sims$Gb)
plot(CC_sims$Ga)
plot(CC_sims$c)
plot(CC_sims$p)


# -- DEFINING DATA
data_list <- list(
  ngroups = ngroups,
  ntrials = ntrials, 
  groupSize = groupSize,
  Ga = Ga,
  c = c)

# -- DEFINING PARAMS
params <- c("omega", "rho", "alpha")

# Fit model
start_time = Sys.time()
samples <- jags(data = data_list,
                inits=NULL,
                parameters.to.save = params,
                model.file ="CC_model_individual.txt", 
                n.chains = 3,
                n.iter=40000, n.burnin=8000, n.thin=1)

end_time = Sys.time()
end_time - start_time

# Save recovered variables
alpha_recov <- array(NA, c(groupSize, ngroups))
rho_recov <- array(NA, c(groupSize, ngroups))
omega_recov <- array(NA, c(groupSize, ngroups))

X <- samples$BUGSoutput$sims.list

for (g in 1:ngroups) {
  
  for (s in 1:groupSize) {
    
    alpha_recov[s,g] <- MPD(X$alpha[,s,g])
    rho_recov[s,g] <- MPD(X$rho[,s,g])
    omega_recov[s,g] <- MPD(X$omega[,s,g])
    
  }
}

# Vectorize
true_alpha <- as.vector(alpha)
true_rho <- as.vector(rho)
true_omega <- as.vector(omega)

recov_alpha <- as.vector(alpha_recov)
recov_rho <- as.vector(rho_recov)
recov_omega <- as.vector(omega_recov)

# Plot the recovery
source("pretty_recov_plot.R")
# straight up recovery
pl1 <- recov_plot(true_alpha, recov_alpha, c("true alpha", "recov alpha"), 'smoothed linear fit')
pl2 <- recov_plot(true_rho, recov_rho, c("true rho", "recov rho"), 'smoothed linear fit')
pl3 <- recov_plot(true_omega, recov_omega, c("true omega", "recov omega"), 'smoothed linear fit')
ggarrange(pl1, pl2, pl3)

# recov as effect of other true parameters...
pl1 <- recov_plot(true_alpha, recov_rho, c("true alpha", "recov rho"), 'smoothed linear fit')
pl2 <- recov_plot(true_alpha, recov_omega, c("true alpha", "recov omega"), 'smoothed linear fit')
pl3 <- recov_plot(true_rho, recov_alpha, c("true rho", "recov alpha"), 'smoothed linear fit')
pl4 <- recov_plot(true_rho, recov_omega, c("true rho", "recov omega"), 'smoothed linear fit')
pl5 <- recov_plot(true_omega, recov_alpha, c("true omega", "recov alpha"), 'smoothed linear fit')
pl6 <- recov_plot(true_omega, recov_rho, c("true omega", "recov rho"), 'smoothed linear fit')
ggarrange(pl1, pl2, pl3, pl4, pl5, pl6)

# recov as effect of other recov parameters...
pl1 <- recov_plot(recov_alpha, recov_rho, c("recov alpha", "recov rho"), 'smoothed linear fit')
pl2 <- recov_plot(recov_alpha, recov_omega, c("recov alpha", "recov omega"), 'smoothed linear fit')
pl3 <- recov_plot(recov_rho, recov_alpha, c("recov rho", "recov alpha"), 'smoothed linear fit')
pl4 <- recov_plot(recov_rho, recov_omega, c("recov rho", "recov omega"), 'smoothed linear fit')
pl5 <- recov_plot(recov_omega, recov_alpha, c("recov omega", "recov alpha"), 'smoothed linear fit')
pl6 <- recov_plot(recov_omega, recov_rho, c("recov omega", "recov rho"), 'smoothed linear fit')
ggarrange(pl1, pl2, pl3, pl4, pl5, pl6)


# Inspection of recovery
# Traceplots
traceplot(samples, mfrow = c(1, 1),"rho")
traceplot(samples)

library(coda)
mcmc_samples <- as.mcmc(samples)

# Convergence diagnostics
gelman.diag(mcmc_samples) # above 1.1 indicates lack of convergence
autocorr.diag(mcmc_samples)
effectiveSize(mcmc_samples)
