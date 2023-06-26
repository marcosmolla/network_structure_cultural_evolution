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
  
    parameters <- expand.grid(n = 1e4,        # Population size, N # USER INPUT
                          g = 7500L,       # Number of generations # USER INPUT
                          r = 1L:100L,      # Number of repetitions # USER INPUT
                          p_r = c(1e0,1e-1,1e-2,0.066,0.033,1e-3,1e-4),      # Rewiring probability for small-world networks, NOTE: -1 is a placeholder for fully connected graphs # USER INPUT
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

load("res.zip")

############################
## Ewens-Watterson test ----
############################
# With the data from the above simulation, we can now perform the EW test. To do this, we will (for each simulation): first take a random sample of size nSam (here 1000) of the N individuals (here 10,000). Then, we use the `get_WFpredictions()` function to compare this sample with one of the same size and with the same number of unique variants (but sampled without bias). This process will create a folder called `WFpredictions` which collects results (so that we only need to calculate them ones). 
# The interesting statistics that are calculated here are the 
#   - `Fcount` (an estimate of the probability that the observed sample has a diversity smaller than samples generated under neutral evolution) and the 
#   - `Ecount` (an estimate of the tail probability that the observed sample has a probability smaller than samples generated under neutral evolution)
#
# Note, this process can take a very long time (especially for large nSam and simMax)! Also, the test requires sufficiently many unique variants in the population. This will most likely not be the case for small populations. Here we used N=1e4.
#

# Run Sampling Function and collect results ----
tim <-Sys.time()
run_again <- TRUE
nSam <- 1e3
simMax <- 1e6
reps <- 1 #1
if(!dir.exists("WFpredictions")) dir.create("WFpredictions")
while(run_again){
 all_wf <- do.call("rbind",
                   mclapply(mc.cores = 8, mc.cleanup=TRUE, RES, function(X){
                     all_obs <- as.numeric(factor(X$record$snapshot_rep))
                     wf<-do.call("rbind", lapply(1:reps, function(i){
                      subst<-sample(x = all_obs, size = nSam, replace=F)
                      mysample <- as.numeric(sort(decreasing = T, table(subst))); mysample
                      get_WFpredictions(mysample = mysample, SIMMAX = simMax) 
                     }))
                     
                     data.frame(n=X$parameters$n,
                                g=X$parameters$g,
                                mu=X$parameters$mu,
                                pr=X$parameters$p_r,
                                r=X$parameters$r,
                                Ecount=wf[,1],
                                Fcount=wf[,2],
                                k = wf[,3])
                    # }
                   })
 )
 
 # Sometimes, because of the multicore usage, one process 'sees' that a results file is already available, while the creating process is still saving and closing the file. This causes an error and returns a character string. If this is not the case, the first value is numeric, in which case we can save the all_wf result and create the results figure. 
 if(is.numeric(all_wf[1,1])){
  save("all_wf", file = "all_wf")
  run_again <- FALSE
  cat("All done!")
 } else {
#   warning("Running code again, as there was a multi-core related issue loading/storing results files.")
  run_again <- TRUE
 }
}
Sys.time() - tim




##############
## Plot Fig 2c
##############

# Load libraries ----
library(dplyr)
library(ggplot2)
library(cowplot); theme_set(theme_cowplot())
library(ggsci)
library(scales)

# Load and prepare data for plotting ----
load("all_wf")
all_wf <- filter(all_wf, !pr%in%c(-1,0))
all_wf$mu <- factor(all_wf$mu, levels=sort(decreasing = T, unique(all_wf$mu)))

## Summarise data and plot ----
alpha <- .05 
all_wf %>% 
 mutate(E_rej_new = Ecount<(alpha/2) | Ecount>(1 - alpha/2),
        F_rej_new = Fcount<(alpha/2) | Fcount>(1 - alpha/2)) %>% 
 group_by(pr, mu) %>% 
 summarise(E_rej_prob = mean(E_rej_new),
           F_rej_prob = mean(F_rej_new),
           meanK = mean(k),
           minK = min(k),
           maxK = max(k),
           sims = length(k)) -> all_wf_s

## PLOT sampling test results ----
ggplot(all_wf_s, aes(col=mu)) +
    geom_point(aes(x=pr, y=E_rej_prob), size=2) +
    geom_line(data=filter(all_wf_s, pr!=-1), aes(x=pr, y=E_rej_prob, group=mu, col=mu), size=1) +
    ggtitle("E rejection prob (Ewens)") +
    geom_hline(yintercept = .05, linetype=2, col="darkgrey") +
    scale_x_continuous(trans  = compose_trans("log10", "reverse"),
                        breaks = scales::trans_breaks("log10", function(x) 10^x),
                        labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    annotation_logticks(sides = "b") +
    scale_color_npg(name=expression(mu)) +
    xlab(expression("Rewiring probability"~p[r])) +
    ylab("") + 
    theme(legend.position = c(.1,.8),
            axis.line = element_blank(),
            panel.border = element_rect(colour = "black",linewidth = 1.2)
            )
