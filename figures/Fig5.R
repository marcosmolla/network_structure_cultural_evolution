#
## Code for 'Implications of network structure for cultural evolution'
#

# Folder directory
dir <- "FOLDER_NAME" # USER INPUT
directory <- paste("FOLDER_DIRECTORY",dir,sep="") # USER INPUT

# Load code from the methods sub directory
setwd(paste(directory,"methods",sep = "/")) 
files.sources = list.files()
tmp<-sapply(files.sources, FUN = source)
setwd('..')

# Libraries to run simulation code:
require(dqrng)
require(igraph)

{
  
    {M <- Inf; MORAN <- T; VTRANS <- T; il <- il_infinite_sites; rec <- record_inf_trait_pop_hash; sl <- sl_freqdep; SL <- "freqdep"}
  
    parameters <- expand.grid(n = 500,        # Population size, N # USER INPUT
                          g = 5000L,       # Number of generations # USER INPUT
                          r = 1L:100L,      # Number of repetitions # USER INPUT
                          p_r = c(-1,1,1e-1,1e-2,0),      # Rewiring probability for small-world networks, NOTE: -1 is a placeholder for fully connected graphs # USER INPUT
                          nei = 5,       # Neighbourhood size of small-world network; standard is 5, results in degree = 10
                          l = 1L,         # Number of memory slots (here, always 1)
                          m = M,          # Number of possible trait variants
                          mu = 1e-3,      # Innovation probability (per newborn) # USER INPUT
                          beta = 1L,      # Social learning probability (here, always 1)
                          d = round(seq(-.1,.1,length.out=11),2),          # Strength and direction of frequency dependent social learning (d<0 anti-conformity, d=0 unbiased, d>0 conformity) # USER INPUT
                          moran = MORAN,  # Whether this is a MORAN process
                          vtrans = VTRANS,# Whether vertical transmission occurs; this is FALSE for all continuous learning
                          sl = SL,        # Social learning process
                          preset=6,  
                          storenet=T      # Whether to store networks
)
}

res <- runSNSL(PARAMETERS = parameters, SIM = simulation, IL = il, SL = sl, REC = rec, DIR=directory, MC=8)



#############
## Plot Fig 5
#############

{
    library(tidyverse)
    library(ggplot2)
    library(cowplot); theme_set(theme_cowplot())
    library(parallel)
    library(igraph)
    load("res.zip")
}
 
bind_rows(mclapply(mc.cores=8, mc.cleanup=TRUE, RES, function(X){
#  bind_rows(lapply(RES, function(X){
 data.frame(
  n = X$parameters$n,
  g = X$parameters$g,
  nei = X$parameters$nei,
  mu = X$parameters$mu,
  d = X$parameters$d,
  p_r = X$parameters$p_r,
  r = X$parameters$r,
  EK = mean(tail(X$record$EK,0.9*length(X$record$EK))))
})) -> dat; head(dat)
saveRDS(dat, "dat")
dat <- readRDS("dat")

dat %>% 
  group_by(mu, d, p_r, n, nei) %>% 
  summarise_at(c("EK"), list(mean=mean, 
                                  sd=sd, 
                                  qlow=~quantile(.,.05), 
                                  qhigh=~quantile(.,.95)) )  -> dat_s
dat_s$p_r <- factor(dat_s$p_r, 
                levels=c(-1,1,.1,.01,.001,.0001,0), 
                labels=c("Fully connected","Random","0.1","0.01","0.001","0.0001","Ring lattice"))

ggplot(dat_s, aes(x=d, col=p_r)) + 
    geom_vline(xintercept = 0, linetype=2, col="black") +
    geom_point(aes(y=mean)) + 
    geom_errorbar(aes(ymin=qlow, ymax=qhigh, x=d, col=p_r), width=.002) + 
    geom_line(aes(y=mean), size=1.1) + 
    ylab("Average number of variants, K") +
    scale_color_viridis_d(name="") + 
    xlab("Strength of frequency dependent bias, d") +
    theme(panel.border = element_rect(colour = "black",size = 1.2),
            axis.line = element_blank(),
            legend.position = c(.62,.86))
