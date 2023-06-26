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
  
    {M <- Inf; MORAN <- T; VTRANS <- T; il <- il_infinite_sites; rec <- record_inf_trait_lastonly; sl <- sl_freqdep; SL <- "freqdep"}
  
    parameters <- expand.grid(n = 1e4,        # Population size, N # USER INPUT
                          g = 2500L,       # Number of generations # USER INPUT
                          r = 1L:100L,      # Number of repetitions # USER INPUT
                          p_r = c(1,.1,.01,0),      # Rewiring probability for small-world networks, NOTE: -1 is a placeholder for fully connected graphs # USER INPUT
                          nei = 5,       # Neighbourhood size of small-world network; standard is 5, results in degree = 10
                          l = 1L,         # Number of memory slots (here, always 1)
                          m = M,          # Number of possible trait variants
                          mu = 1e-3,      # Innovation probability (per newborn) # USER INPUT
                          beta = 1L,      # Social learning probability (here, always 1)
                          d = c(-.1, 0, .1),          # Strength and direction of frequency dependent social learning (d<0 anti-conformity, d=0 unbiased, d>0 conformity) # USER INPUT
                          moran = MORAN,  # Whether this is a MORAN process
                          vtrans = VTRANS,# Whether vertical transmission occurs; this is FALSE for all continuous learning
                          sl = SL,        # Social learning process
                          preset=5,  
                          storenet=T      # Whether to store networks
)
}

res <- runSNSL(PARAMETERS = parameters, SIM = simulation, IL = il, SL = sl, REC = rec, DIR=directory, MC=8)



#############
## Plot Fig 4
#############

{
  library(tidyverse)
  library(ggplot2)
  library(cowplot); theme_set(theme_cowplot())
  library(scico) # more colours
  library(igraph)
  library(parallel)
  
  load("res.zip")
}



### Using Simpson's index

res <- RES
hist_data<-bind_rows(
#   mclapply(mc.cores=8, mc.cleanup=T, res, function(X){
  lapply(res, function(X){
    g <- length(X$record$EK)
    net <- X$parameters$p_r
    d <- X$parameters$d
    n <- X$parameters$n
    r <- X$parameters$r
    TIME<-1:g

    rep_all <- as.numeric(X$record$snapshot_rep)

    if(net!=-1){
      G <- graph_from_edgelist(X$record$edgel)
      adjm <- as.matrix(get.adjacency(G))
      diag(adjm) <- 1 # include a self-reference
      prop_same_neibs <- mapply(1:length(rep_all), FUN = function(ind){
        neibs <- which(adjm[ind, ]==1)
        simpson(rep_all[neibs])
      })
    } else {
      prop_same_neibs <- rep(simpson(rep_all), n)
    }

    prop_same_neibs_tt <- as.data.frame(prop_same_neibs)
    prop_same_neibs_tt$id <- paste(net, d, r,sep="_")
    prop_same_neibs_tt$net <- net

    pivot_longer(data=prop_same_neibs_tt,
                 cols = !c("net","id"),
                 # names_to = "ind",
                 # names_transform = list(ind = as.integer),
                 values_to = "prop_same_neibs") -> prop_same_neibs_tt_p


    # Adding network metrics
    if(net==-1){
      clu <- rep(x = 1, n)
      deg <- n-1
      pat <- 1
      trn <- 1
    } else {
      clu <- transitivity(graph = G, type = "local", isolates = "zero") # this 
      deg <- degree(G, mode="in")
      pat <- average.path.length(G)
      trn <- transitivity(graph = G, type = "global", isolates = "zero") # this 
    }

    hd <- data.frame(prop_same_neibs_tt_p)
    hd$d <- d
    hd$r <- r
    hd$clu <- clu
    hd$deg <- deg
    hd$pat <- pat
    hd$trn <- trn
    return(hd)
  })
)
saveRDS(hist_data, "hist_data")
hist_data <- readRDS("hist_data")
hist_data$net <- factor(hist_data$net, levels=c(1, .1, .01, 0))


hist_data %>% group_by(net, d) %>% summarise(meanSim = mean(prop_same_neibs), meanTrn = mean(trn)) -> hdu_s_s
ggplot(hdu_s_s) + 
  # geom_hline(yintercept = 1, linetype=2, col="lightgrey") +
  geom_line(aes(x=meanTrn, y=meanSim, col=factor(d, levels=c(-.1,0,.1)))) +
  geom_point(aes(x=meanTrn, y=meanSim, col=factor(d, levels=c(-.1,0,.1)), shape=factor(net, levels=c(1,.1,.01,0)) ), size=3) + #
  scale_color_manual(name="d", values=c("#e27c7c", "#333333", "#6cd4c5"), guide="none") +
  scale_shape_manual(name=bquote(p[r]), values=c(16,17,15,7)) +
  scale_y_continuous(breaks=seq(0,1,.2)) +
  xlab("Average transitivity") +
  ylab("Average Simpson's index") +
  annotate("text", x=.06, y=.95, label= "d = 0.1") + 
  annotate("text", x=.06, y=.18, label= "d = 0   ") + 
  annotate("text", x=.06, y=.02, label= "d = -0.1") + 
  theme(
    legend.title = element_text(size=14),
    legend.text = element_text(size=11),
    legend.key.size = unit(1, "mm"),
    legend.margin = margin(t = 4, unit='mm'),
    legend.spacing.y = unit(1,"mm"),
    legend.position = c(.05,.58), 
    strip.background = element_blank(),
    panel.border = element_rect(colour = "black",linewidth = 1.2),
    axis.line = element_blank()) 
