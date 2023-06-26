#
## Code for 'Implications of network structure for cultural evolution'
#

# Simulation function that sets up the environment (repertoire, network, reporting objects) and runs all timesteps of learning and death-birth replacement ----
setGeneric('simulation', function(SETUP, IL, SL, REC, REPETITION=1) standardGeneric('simulation'))
setMethod(f = 'simulation', signature = c(SETUP="data.frame", IL="function", SL="function", REC="function", REPETITION="ANY"), definition = function(SETUP, IL, SL, REC, REPETITION){
 # Set up repertoire
 repertoire <- set_repertoire(S = SETUP)
 # Set up network
 adjm <- init_adjm(S = SETUP)
 # Set up data recording
 rec <- REC(S = SETUP, INDEX = NULL, ADJ = adjm, REP=repertoire, NEW=NULL)
 # Start simulation loop
 for(gen in 1:(SETUP$"g"*SETUP$"n")){
  # Select newborn and parent
  newborn_parent <- selection_neutral(S = SETUP)
  # Let newborn learn
  repertoire <- learn_single_trait(NP= newborn_parent, ADJ = adjm, REP = repertoire, S = SETUP, IL = IL, SL = SL)
  # Record simulation results
  rec <- REC(S = SETUP, INDEX = gen, REP = repertoire, ADJ = adjm, REC = rec, NEW = newborn_parent[1])
 }
 # Collect simulation parameters and results
 sim_res <- list(parameters = data.frame(repetition = REPETITION,
                                         SETUP),
                 record = rec)
 return(sim_res)
})
