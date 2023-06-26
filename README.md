# Code and Material for Implications of network structure for cultural evolution

This is the supplemental repository for the manuscript [Implications of network structure for cultural
evolution](https://github.com/marcosmolla/network_structure_cultural_evolution)

## File overview

- `model` folder contains the simulation code
- `figures` folder contains the plotting code for the figures of the main text
- `simulate_lifetime` folder contains a `Julia` script that is used to simulate a variant's survival time for different network configurations (use this for the results in Fig. 2a of the main manuscript)

## Instructions for working with the simulation model

The `input.R` file sets up the simulation you would like to run. Look out for the `# USER INPUT` tags, which indicate lines where you can or need to make adjustments to let a specific simulation run on your computer (or cluster).

The `run.R` file is a wrapper that is called from within `input.R`. If a `directory` has been provided, this function will test for the existence of this folder and create one if it does not exist. If a directory is provided this function will also run the simulation on more than one CPU core (or cluster nodes). The number of cores is determined based on the `MC` parameter of the `runSNSL()` function. 

The `simulation.R` file contains the code to run a single simulation. It setups the population (repertoire) and social network, and a data recording variable. Then, a `for` loop runs through the following steps:

1. Selection
2. Learning
3. Data recording

At the end of the simulation, both the recorded data as as well as the set of parameters of this simulation are returned as a list object.

At the end of all simulations, the results will be returned as a list object. (In addition, if running on multiple cores, the results data will also be stored as `res.zip` file in the working directory).

Working with the data requires to work with listed objects, which is described in detail in the `figures` folder.

To recreate the figures from the main text, run the code from the respective files in the `figures` folder. Note that some of these simulations will run for a very long time. Try to run them with fewer repetitions and generations first. 

### Requirements to run the code

The simulation model was tested with R 4.2.2 and requires the following packages to run: `dqrng, igraph, parallel`

Data analysis and plotting requires further packages: `tidyverse, ggsci, viridis, ggplot2, cowplot`

**Note** To generate the results from Figure 2a requires Julia 1.8 and the following packages: `Statistics, Distributed, Graphs, RCall, SharedArrays, Graphs`

### Simulations reported in our article
In the `figures` folder we provide the simulation and plotting code that is necessary to recreate the figures from the main text.

**Note** The simulation code that is provided in the `figures` folder might be running for a very long time (it is `R` after all). So make sure to first try running the simulations with the $N$ you are interested in but with fewer repetitions and only few generations. You can record the time the simulation runs using `Sys.time()` and extrapolate from there (the time the entire set of simulations will run increases linearly).

## Additional material 

[Shiny App to calculate expected K and T](https://smolla.shinyapps.io/Expected_K_and_T/)
