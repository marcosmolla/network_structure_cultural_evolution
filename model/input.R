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
# First, choose the mode of social learning by changing the integer in the square brackets below:
       # 1: random (unbiased), 
       # 2: frequency biased, or 
       # 3: random (in the innovation diffusion model) 

       SL <- c("rand", "freqdep", "randdiff")[1]  # USER INPUT

       if(SL=="rand")      sl <- sl_random_oblique
       if(SL=="freqdep")   sl <- sl_freqdep
       if(SL=="randdiff")  sl <- sl_random_oblique_diffusion
# Then, choose one of the following pre-sets
       # 1: 2_trait_many: unbiased, two traits, Moran replacement, recording every N timesteps
       # 2: 2_trait_large: unbiased, two traits, Moran replacement, recording every N timesteps complete repertoire snapshot
       # 3: inf_traits_many: unbiased, infinitely many, Moran replacement, recording every N timesteps
       # 4: inf_traits_large: unbiased, infinitely many, Moran replacement, recording every N timesteps complete repertoire snapshot
       # 5: inf_traits_lastonly: unbiased, infinitely many, Moran replacement, recording K every N timesteps and complete repertoire at last step
       # 6: inf_traits_hash: unbiased, infinitely many, Moran replacement, recording every timestep traits in hash table
       # 7: innovation diffusion of a novel trait, no death simulation
       # 8: innovation diffusion of a novel trait, Moran replacement
       # 9: innovation diffusion of a novel trait, Moran replacement, recording every N timesteps complete repertoire snapshot
       
       preset <- 1 # USER INPUT

       if(preset==1) {M <- 2L;  MORAN <- T; VTRANS <- F; il <- il_two_trait;      rec <- record_2trait}
       if(preset==2) {M <- 2L;  MORAN <- T; VTRANS <- F; il <- il_two_trait;      rec <- record_2trait_pop_large}
       if(preset==3) {M <- Inf; MORAN <- T; VTRANS <- T; il <- il_infinite_sites; rec <- record_inf_trait_pop}
       if(preset==4) {M <- Inf; MORAN <- T; VTRANS <- T; il <- il_infinite_sites; rec <- record_inf_trait_pop_large}
       if(preset==5) {M <- Inf; MORAN <- T; VTRANS <- T; il <- il_infinite_sites; rec <- record_inf_trait_lastonly}
       if(preset==6) {M <- Inf; MORAN <- T; VTRANS <- T; il <- il_infinite_sites; rec <- record_inf_trait_pop_hash}
       
       if(preset==7) {M <- 2L;   MORAN <- F; VTRANS <- F; il <- il_diffusion; rec <- record_2trait}
       if(preset==8) {M <- 2L;   MORAN <- T; VTRANS <- F; il <- il_diffusion; rec <- record_2trait}
       if(preset==9) {M <- 2L;   MORAN <- T; VTRANS <- F; il <- il_diffusion; rec <- record_2trait_pop_large}
 
parameters <- expand.grid(n = 500,        # Population size, N # USER INPUT
                          g = 5e3L,       # Number of generations # USER INPUT
                          r = 1L:2L,      # Number of repetitions # USER INPUT
                          p_r = c(-1,1,1e-1,1e-2,1e-3,0),      # Rewiring probability for small-world networks, NOTE: -1 is a placeholder for fully connected graphs # USER INPUT
                          nei = 5L,       # Neighbourhood size of small-world network; standard is 5, results in degree = 10
                          l = 1L,         # Number of memory slots (here, always 1)
                          m = M,          # Number of possible trait variants
                          mu = 1e-3,      # Innovation probability (per newborn) # USER INPUT
                          beta = 1L,      # Social learning probability (here, always 1)
                          d = 0,          # Strength and direction of frequency dependent social learning (d<0 anti-conformity, d=0 unbiased, d>0 conformity) # USER INPUT
                          moran = MORAN,  # Whether this is a MORAN process
                          vtrans = VTRANS,# Whether vertical transmission occurs; this is FALSE for all continuous learning
                          sl = SL,        # Social learning process
                          preset=preset,  
                          storenet=T      # Whether to store networks
)
}


##################################################
## Run repeated simulations for several parameters
##################################################
# Uncomment one of the following lines to execute the simulation 
# For single simulation runs (does not save results)
       # res <- runSNSL(PARAMETERS = parameters, SIM = simulation, IL = il, SL = sl, REC = rec) # USER INPUT
# Run many simulations on more than one core (currently set to 8), results will be saved in directory (will be created if not existing)
       # res <- runSNSL(PARAMETERS = parameters, SIM = simulation, IL = il, SL = sl, REC = rec, DIR=directory, MC=8) # USER INPUT