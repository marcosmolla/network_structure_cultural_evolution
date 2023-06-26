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
                          g = 5e4L,       # Number of generations # USER INPUT
                          r = 1L:3L,      # Number of repetitions # USER INPUT
                          p_r = c(-1,1e0,1e-1,1e-2,1e-3,1e-4,0),      # Rewiring probability for small-world networks, NOTE: -1 is a placeholder for fully connected graphs # USER INPUT
                          nei = 5L,       # Neighbourhood size of small-world network; standard is 5, results in degree = 10
                          l = 1L,         # Number of memory slots (here, always 1)
                          m = M,          # Number of possible trait variants
                          mu = c(1e-2, 1e-3),      # Innovation probability (per newborn) # USER INPUT
                          beta = 1L,      # Social learning probability (here, always 1)
                          d = 0,          # Strength and direction of frequency dependent social learning (d<0 anti-conformity, d=0 unbiased, d>0 conformity) # USER INPUT
                          moran = MORAN,  # Whether this is a MORAN process
                          vtrans = VTRANS,# Whether vertical transmission occurs; this is FALSE for all continuous learning
                          sl = SL,        # Social learning process
                          preset=2,  
                          storenet=F      # Whether to store networks
)
}

res <- runSNSL(PARAMETERS = parameters, SIM = simulation, IL = il, SL = sl, REC = rec, DIR=directory, MC=8)



##############
## Plot Fig 2b
##############
library(tidyverse)
library(ggsci)
library(ggplot2)
library(cowplot); theme_set(theme_cowplot())

### Extract the relevant information from our results file
{
  load(paste(directory,"res.zip",sep="/"))
  ## Setting up data ----
  bind_rows(lapply(RES, function(X){
    param <- data.frame(
      n = X$parameters$n,
      g = X$parameters$g,
      nei = X$parameters$nei,
      mu = X$parameters$mu,
      d = X$parameters$d,
      p_r = X$parameters$p_r,
      r = X$parameters$repetition
    )
    EK = mean(tail(X$record$EK,0.5*length(X$record$EK)))
    return(cbind(param, EK=EK, X$record$ET))
  })) -> dat; head(dat)
}
saveRDS(dat, file = "summarised_dat"); rm(dat)
dat <- readRDS("summarised_dat")
dat$p_r <- factor(dat$p_r, levels=c(-1,1e-0,1e-1,1e-2,1e-3,1e-4,0), labels=c("Full","Rand",0.1,0.01,0.001,0.0001,"Ring"))

### Summarise data
dat %>% group_by(mu, p_r) %>% summarise(meanEK=mean(EK),
                                                lowerEK=confint(lm(EK~1), level = 0.9)[1],
                                                higherEK=confint(lm(EK~1), level = 0.9)[2]) -> dat_s
dat_s$mu <- factor(dat_s$mu, levels=c(1e-2,1e-3))

### Plot 
ggplot(dat_s) + 
  geom_hline(yintercept = c(E_K(500, 0.01), E_K(500, 0.001)), linetype=2, col=c("#E64B35FF","#4DBBD5FF")) + 
  geom_point(aes(x=p_r, y=meanEK, col=(mu)), size=2, alpha=1) +
  scale_y_continuous(limits=c(0,NA), breaks=c(0,10,20,30)) +
  ylab("Average number of variants, K") +
  scale_x_discrete(labels=c("Full", expression(10^0), expression(10^-1), expression(10^-2), expression(10^-3), expression(10^-4), 0)) +
  scale_color_npg(name=expression(mu)) +
  xlab(expression("Rewiring probability"~p[r])) +
  theme(panel.border = element_rect(colour = "black",linewidth = 1.2),
        axis.line = element_blank(),
        legend.position = c(.05,.87)) 
