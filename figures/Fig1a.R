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

# Setting parameters for the simulation 
{
       {M <- 2; il <- il_two_trait;      rec <- record_2trait_pop_large; SL <- "rand"; sl <- sl_random_oblique}
 
parameters <- expand.grid(n = 500,        # Population size, N # USER INPUT
                          g = 5e3L,       # Number of generations # USER INPUT
                          r = 1L:1L,      # Number of repetitions # USER INPUT
                          p_r = c(-1,1,1e-1,1e-2,0),      # Rewiring probability for small-world networks, NOTE: -1 is a placeholder for fully connected graphs # USER INPUT
                          nei = 5L,       # Neighbourhood size of small-world network; standard is 5, results in degree = 10
                          l = 1L,         # Number of memory slots (here, always 1)
                          m = M,          # Number of possible trait variants
                          mu = 1e-3,      # Innovation probability (per newborn) # USER INPUT
                          beta = 1L,      # Social learning probability (here, always 1)
                          d = 0,          # Strength and direction of frequency dependent social learning (d<0 anti-conformity, d=0 unbiased, d>0 conformity) # USER INPUT
                          moran = T,  # Whether this is a MORAN process
                          vtrans = T,# Whether vertical transmission occurs; this is FALSE for all continuous learning
                          sl = SL,        # Social learning process
                          preset=2,  
                          storenet=F      # Whether to store networks
)
}


##################################################
## Run repeated simulations for several parameters
##################################################
# Run many simulations on more than one core (currently set to 8), results will be saved in directory (will be created if not existing)
       res <- runSNSL(PARAMETERS = parameters, SIM = simulation, IL = il, SL = sl, REC = rec, DIR=directory, MC=8) # USER INPUT


##############
## Plot Fig 1a
##############
library(tidyr)
library(dplyr)
library(ggsci)
library(ggplot2)
library(cowplot); theme_set(theme_cowplot())

load("res.zip"); res <- RES
pl<-lapply(1:length(res), function(i){
    g <- length(res[[i]]$record$trait_pop)
    net <- res[[i]]$parameters$p_r
    TIME<-1:g
    
    # Histogram
    sim_at_mean_s <- which(res[[i]]$record$trait_pop==.5 & (1:g)>=(g/2)) |> mySample(1)
    if(length(sim_at_mean_s)==0) sim_at_mean_s <- sample((1:g)>=(g/2), 1)

    prop_same_neibs_tt <- data.frame(vals = res[[i]]$record$prop_in_neib[g,])
    prop_same_neibs_tt$id <- sim_at_mean_s
    prop_same_neibs_tt$net <- net
    
    pivot_longer(data=prop_same_neibs_tt, 
                 cols = !c("net","id"), 
                 # names_to = "ind",
                 # names_transform = list(ind = as.integer),
                 values_to = "prop_same_neibs") -> prop_same_neibs_tt_p
    
    col <- pal_npg()(5)[which(c(-1,1,0,1e-1,1e-2)==net)]
    p1<-
      ggplot(data.frame(prop_same_neibs_tt_p)) + 
      geom_histogram(aes(prop_same_neibs), fill=alpha(col,.5), col=col, binwidth = .1) +
      facet_wrap("id", ncol=1) +
      scale_x_continuous(limits=c(-.1,1.11), breaks=seq(0,1,.2)) +
      xlab("Prop. of neighbours with same trait") + ylab("Count") +
      theme(
        strip.background = element_blank(),
        strip.text = element_blank(),
        # axis.text.y = element_blank(),
        panel.border = element_rect(colour = "black", linewidth = 1.2),
        axis.line = element_blank())
    
    # return(
        plot_grid(p1 + ggtitle(ifelse(net==-1, "Fully connected", paste("p_r =",net)))) 
    # )
  })
plot_grid(pl[[1]],pl[[2]],pl[[3]],pl[[4]],pl[[5]], nrow=1, labels="a")
