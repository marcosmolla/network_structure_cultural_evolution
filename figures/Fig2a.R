# The following code produces Figure 2a from the main text. 
# It requires the simulation results from the simulate_lifetime.jl (in the simulate_lifetime folder)

{
  directory <- "FOLDER_DIRECTORY"
  setwd(directory)

  library(tidyverse)
  library(ggplot2)
  library(cowplot); theme_set(theme_cowplot())
  library(ggsci)
}

##############
## Plot Fig 2a
##############

data <- readRDS("data_simple_sim")
data$p_r <- factor(data$p_r, levels = c(-1,1e0, 1e-1, 1e-2, 1e-3, 1e-4, 0), labels=c("Full","10^0", "10^-1", "10^-2", "10^-3", "10^-4", "0"))

data %>% group_by(N,K,p_r) %>% summarise(meanT=mean(t),
                                         lowerT=confint(lm(t~1), level = .9)[1],
                                         upperT=confint(lm(t~1), level = .9)[2],
                                         medianT=median(t), 
                                         numFix=sum(fixation),
                                         numLoss=sum(!fixation),
                                         propFix=numFix/n(), .groups = "drop") -> data_m


ggplot(data_m) + 
  geom_hline(yintercept = 3359, linetype=2, col="lightgrey") +
  geom_point(aes(x=p_r, y=meanT)) +
  geom_errorbar(aes(ymin=lowerT, ymax=upperT, x=p_r), width=.07) +
  ylab("Average variant life time, T") +
  xlab(expression("Rewiring probability,"~p[r])) +
  scale_x_discrete(labels=c("Full", expression(10^0), expression(10^-1), expression(10^-2), expression(10^-3), expression(10^-4), 0)) +
  theme(panel.border = element_rect(colour = "black",linewidth = 1.2),
        axis.line = element_blank(),
        strip.background = element_blank())
