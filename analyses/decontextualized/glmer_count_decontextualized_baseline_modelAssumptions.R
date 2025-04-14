
# LOADING PACKAGES
if(!require("pacman")) install.packages("pacman")
library("pacman")
p_load("dplyr","lme4","ggplot2","readr","readxl","lmerTest","optimx","psych",
       "corrplot","DHARMa")

# ______________________________________________________________________________
# ____________________ MODEL ASSUMPTIONS: Baseline models ______________________

##### check negative Binomial models assumptions with DHARMa

# ------------------------------------------------------------------------------

plot_folder = ".../decontextualized/models_assumptions_plots/baseline_models"
model_folder = ".../decontextualized/results_baseline_models"
models_names <- c("baseline_vecDiffs","baseline_vecs","baseline_vecs_CAOSS")

# (1) load models
load(paste(model_folder,"/",models_names[1],".rda",sep = ""))
load(paste(model_folder,"/",models_names[2],".rda",sep = ""))
load(paste(model_folder,"/",models_names[3],".rda",sep = ""))

zeroinflation <- dispersion <- list()


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# (2.1) vector difference
    
# simulate residuals (default unconditional simulation: simulate across all 
# hierarchical levels)
set.seed(5)
sim1_negBinomial <- simulateResiduals(fittedModel = bas_vecDiffs,
                                      n = 250, plot = F)

x <- testZeroInflation(sim1_negBinomial) # zero inflation
zeroinflation <- append(zeroinflation,x$statistic)

png(paste(plot_folder,"QQplot_",models_names[1],".png",sep = ""), height = 10, width = 10, units = "cm", res = 400)
plotQQunif(sim1_negBinomial) # QQ-plot residuals
dev.off()

png(paste(plot_folder,"Quantiles_",models_names[1],".png",sep = ""), height = 10, width = 10, units = "cm", res = 400)
testQuantiles(sim1_negBinomial) # residual plot equipped with regression lines that are either solid (actual quantiles) or dashed (theoretical quantiles)
dev.off()

# NOTE: following package recommendations, the default test of dispersion is 
# used with *conditional* simulations (i.e., conditional on random effect 
# structure)
sim1_negBinomial_conditional <- simulateResiduals(fittedModel = bas_vecDiffs,
                                                  n = 250, plot = F, use.u = T)
x <- testDispersion(sim1_negBinomial_conditional) # over/underdispersion
dispersion <- append(dispersion,x$statistic)
    

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# (2.2) additive model

# simulate residuals (default unconditional simulation: simulate across all 
# hierarchical levels)
set.seed(5)
sim1_negBinomial <- simulateResiduals(fittedModel = bas_vecs,
                                      n = 250, plot = F)

x <- testZeroInflation(sim1_negBinomial) # zero inflation
zeroinflation <- append(zeroinflation,x$statistic)

png(paste(plot_folder,"QQplot_",models_names[2],".png",sep = ""), height = 10, width = 10, units = "cm", res = 400)
plotQQunif(sim1_negBinomial) # QQ-plot residuals
dev.off()

png(paste(plot_folder,"Quantiles_",models_names[2],".png",sep = ""), height = 10, width = 10, units = "cm", res = 400)
testQuantiles(sim1_negBinomial) # residual plot equipped with regression lines that are either solid (actual quantiles) or dashed (theoretical quantiles)
dev.off()

# NOTE: following package recommendations, the default test of dispersion is 
# used with *conditional* simulations (i.e., conditional on random effect 
# structure)
sim1_negBinomial_conditional <- simulateResiduals(fittedModel = bas_vecs,
                                                  n = 250, plot = F, use.u = T)
x <- testDispersion(sim1_negBinomial_conditional) # over/underdispersion
dispersion <- append(dispersion,x$statistic)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# (2.3) CAOSS model


# simulate residuals (default unconditional simulation: simulate across all 
# hierarchical levels)
set.seed(5)
sim1_negBinomial <- simulateResiduals(fittedModel = bas_vecs_CAOSS,
                                      n = 250, plot = F)

x <- testZeroInflation(sim1_negBinomial) # zero inflation
zeroinflation <- append(zeroinflation,x$statistic)

png(paste(plot_folder,"QQplot_",models_names[3],".png",sep = ""), height = 10, width = 10, units = "cm", res = 400)
plotQQunif(sim1_negBinomial) # QQ-plot residuals
dev.off()

png(paste(plot_folder,"Quantiles_",models_names[3],".png",sep = ""), height = 10, width = 10, units = "cm", res = 400)
testQuantiles(sim1_negBinomial) # residual plot equipped with regression lines that are either solid (actual quantiles) or dashed (theoretical quantiles)
dev.off()

# NOTE: following package recommendations, the default test of dispersion is 
# used with *conditional* simulations (i.e., conditional on random effect 
# structure)
sim1_negBinomial_conditional <- simulateResiduals(fittedModel = bas_vecs_CAOSS,
                                                  n = 250, plot = F, use.u = T)
x <- testDispersion(sim1_negBinomial_conditional) # over/underdispersion
dispersion <- append(dispersion,x$statistic)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# (3) export summary

D = data.frame(matrix(cbind(unlist(zeroinflation),unlist(dispersion)),nrow = 3))
rownames(D) <- models_names
colnames(D) <- c("zero_inflation","dispersion")
save(D, file = paste(model_folder,"/assumptions_baseline_models.rda", sep = ""))


