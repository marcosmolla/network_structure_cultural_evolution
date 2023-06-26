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
  
    {M <- Inf; MORAN <- T; VTRANS <- T; il <- il_infinite_sites; rec <- record_inf_trait_lastonly; sl <- sl_random_oblique; SL <- "rand"}
  
    parameters <- expand.grid(n = 500,        # Population size, N # USER INPUT
                          g = 5e4L,       # Number of generations # USER INPUT
                          r = 1L:100L,      # Number of repetitions # USER INPUT
                          p_r = c(1,.1,.01,.001,0),      # Rewiring probability for small-world networks, NOTE: -1 is a placeholder for fully connected graphs # USER INPUT
                          nei = c(5,10,20,50,100),       # Neighbourhood size of small-world network; standard is 5, results in degree = 10
                          l = 1L,         # Number of memory slots (here, always 1)
                          m = M,          # Number of possible trait variants
                          mu = 1e-3,      # Innovation probability (per newborn) # USER INPUT
                          beta = 1L,      # Social learning probability (here, always 1)
                          d = 0,          # Strength and direction of frequency dependent social learning (d<0 anti-conformity, d=0 unbiased, d>0 conformity) # USER INPUT
                          moran = MORAN,  # Whether this is a MORAN process
                          vtrans = VTRANS,# Whether vertical transmission occurs; this is FALSE for all continuous learning
                          sl = SL,        # Social learning process
                          preset=5,  
                          storenet=F      # Whether to store networks
)
}

res <- runSNSL(PARAMETERS = parameters, SIM = simulation, IL = il, SL = sl, REC = rec, DIR=directory, MC=8)



##########################
## Plot simulation results
##########################
{
  load("res.zip")
  library(tidyverse)
  library(ggplot2)
  library(cowplot); theme_set(theme_cowplot())
}

lapply(RES, function(X){
  return(data.frame(select(X$parameters, n, r, p_r, mu, d, nei),
                    K = mean(tail(X$record$EK, X$parameters$g*0.5)),
                    lambdaGlobal = X$record$lambdaGlobal,
                    lambdaLocal = mean(X$record$lambdaLocal),row.names = NULL
  ))
}) %>% bind_rows -> dat
dat %>% group_by(nei,p_r) %>% summarise(counts=n()) %>% as.data.frame()
dat$p_r <- factor(dat$p_r, levels = c(1,.1,.01,.001,0), labels = c("1",".1",".01",".001","0"))


dat %>% group_by(nei,p_r) %>% summarise(meanK=mean(K), meanGlobal=mean(lambdaGlobal), meanLocal=mean(lambdaLocal), .groups = "drop") %>% rowwise %>% mutate(overlap=min(meanGlobal,meanLocal)/max(meanGlobal,meanLocal)) -> dat_overlap
ggplot(data=dat_overlap, aes(x=nei*2, y=overlap, shape=p_r)) + 
  geom_hline(yintercept = 1, col="lightgrey", linetype=2) +
  geom_point(size=2) + geom_line(linewidth=.3) +
  expand_limits(x=0) +
  scale_shape(name=bquote(p[r])) +
  xlab("Average degree, k") +
  ylab("Overlap local and global Lambda") +
  theme(panel.border = element_rect(colour = "black",linewidth = 1.2),
        axis.line = element_blank(),
        strip.background = element_blank(),
        legend.position = c(.7,.65)) -> q_overlap

dat %>% group_by(nei, p_r) %>% summarise(meanK=mean(K)) -> dat_s
qq <- ggplot(dat_s) +
  expand_limits(x=0, y=3.8) +
  geom_hline(yintercept = E_K(500, 1e-3), col="lightgrey", linetype=2) +
  geom_point(aes(x=nei*2, y=meanK, shape=p_r), size=2) + 
  geom_line(aes(x=nei*2, y=meanK, group=p_r), linewidth=.3) + 
  xlab("Average degree, k") +
  ylab("Average number of variants, K") +
#   scale_color_npg(name=bquote(p[r])) +
  scale_shape(name=bquote(p[r])) +
  theme(panel.border = element_rect(colour = "black",linewidth = 1.2),
        axis.line = element_blank(),
        legend.position = c(.7,.7))

plot_grid(q_overlap, qq, labels=c("(a)","(b)"), label_fontface="plain", hjust=0 , nrow=1)
