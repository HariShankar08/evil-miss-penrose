
# LOADING PACKAGES
if(!require("pacman")) install.packages("pacman")
library("pacman")
p_load("dplyr","lme4","ggplot2","readr","readxl","lmerTest","optimx","psych",
       "corrplot","DHARMa")

# ______________________________________________________________________________
# ____________________ (2) MODEL ASSUMPTIONS: Llama-2-13b ______________________

##### check negative Binomial models assumptions with DHARMa

# ------------------------------------------------------------------------------
# (2.1) Llama-2-13b

plot_folder = ".../decontextualized/models_assumptions_plots/llama_original/"
model_folder = ".../decontextualized/results_llama_original/"
zeroinflation <- dispersion <- list()

suppressMessages({
  for (layer in 1:40){
    print(paste("layer",layer))
    
    # load model 
    load(paste(model_folder,"negBinomial_llama_original_L",layer,".rda",sep = ""))
    
    # simulate residuals (default unconditional simulation: simulate across all 
    # hierarchical levels)
    set.seed(5)
    sim1_negBinomial <- simulateResiduals(fittedModel = m,
                                          n = 250, plot = F)
    
    x <- testZeroInflation(sim1_negBinomial) # zero inflation
    zeroinflation <- append(zeroinflation,x$statistic)
    
    png(paste(plot_folder,"QQplot_layer",layer,".png",sep = ""), height = 10, width = 10, units = "cm", res = 400)
    plotQQunif(sim1_negBinomial) # QQ-plot residuals
    dev.off()
    
    png(paste(plot_folder,"Quantiles_layer",layer,".png",sep = ""), height = 10, width = 10, units = "cm", res = 400)
    testQuantiles(sim1_negBinomial) # residual plot equipped with regression lines that are either solid (actual quantiles) or dashed (theoretical quantiles)
    dev.off()
    
    # NOTE: following package recommendations, the default test of dispersion is 
    # used with *conditional* simulations (i.e., conditional on random effect 
    # structure)
    sim1_negBinomial_conditional <- simulateResiduals(fittedModel = m,
                                                      n = 250, plot = F, use.u = T)
    x <- testDispersion(sim1_negBinomial_conditional) # over/underdispersion
    dispersion <- append(dispersion,x$statistic)
    
  }
})

D = data.frame(matrix(cbind(unlist(zeroinflation),unlist(dispersion)),nrow = 40))
rownames(D) <- paste("layer",seq(1,40))
colnames(D) <- c("zero_inflation","dispersion")
save(D, file = paste(model_folder,"assumptions_llama_original.rda", sep = ""))



# ------------------------------------------------------------------------------
# (2.2) Llama-2-13b - cwe averaged over 10 sentences

plot_folder = ".../decontextualized/models_assumptions_plots/llama_original_10/"
model_folder = ".../decontextualized/results_llama_original_10/"
zeroinflation <- dispersion <- list()

suppressMessages({
  for (layer in 1:40){
    print(paste("layer",layer))
    
    # load model 
    load(paste(model_folder,"negBinomial_llama_original10_L",layer,".rda",sep = ""))
    
    # simulate residuals (default unconditional simulation: simulate across all 
    # hierarchical levels)
    set.seed(5)
    sim1_negBinomial <- simulateResiduals(fittedModel = m,
                                          n = 250, plot = F)
    
    x <- testZeroInflation(sim1_negBinomial) # zero inflation
    zeroinflation <- append(zeroinflation,x$statistic)
    
    png(paste(plot_folder,"QQplot_layer",layer,".png",sep = ""), height = 10, width = 10, units = "cm", res = 400)
    plotQQunif(sim1_negBinomial) # QQ-plot residuals
    dev.off()
    
    png(paste(plot_folder,"Quantiles_layer",layer,".png",sep = ""), height = 10, width = 10, units = "cm", res = 400)
    testQuantiles(sim1_negBinomial) # residual plot equipped with regression lines that are either solid (actual quantiles) or dashed (theoretical quantiles)
    dev.off()
    
    # NOTE: following package recommendations, the default test of dispersion is 
    # used with *conditional* simulations (i.e., conditional on random effect 
    # structure)
    sim1_negBinomial_conditional <- simulateResiduals(fittedModel = m,
                                                      n = 250, plot = F, use.u = T)
    x <- testDispersion(sim1_negBinomial_conditional) # over/underdispersion
    dispersion <- append(dispersion,x$statistic)
    
  }
})

D = data.frame(matrix(cbind(unlist(zeroinflation),unlist(dispersion)),nrow = 40))
rownames(D) <- paste("layer",seq(1,40))
colnames(D) <- c("zero_inflation","dispersion")
save(D, file = paste(model_folder,"assumptions_llama_original10.rda", sep = ""))


# ------------------------------------------------------------------------------
# (2.3) Llama-2-13b - cwe averaged over 5 sentences

plot_folder = ".../decontextualized/models_assumptions_plots/llama_original_5/"
model_folder = ".../decontextualized/results_llama_original_5/"
zeroinflation <- dispersion <- list()

suppressMessages({
  for (layer in 1:40){
    print(paste("layer",layer))
    
    # load model 
    load(paste(model_folder,"negBinomial_llama_original5_L",layer,".rda",sep = ""))
    
    # simulate residuals (default unconditional simulation: simulate across all 
    # hierarchical levels)
    set.seed(5)
    sim1_negBinomial <- simulateResiduals(fittedModel = m,
                                          n = 250, plot = F)
    
    x <- testZeroInflation(sim1_negBinomial) # zero inflation
    zeroinflation <- append(zeroinflation,x$statistic)
    
    png(paste(plot_folder,"QQplot_layer",layer,".png",sep = ""), height = 10, width = 10, units = "cm", res = 400)
    plotQQunif(sim1_negBinomial) # QQ-plot residuals
    dev.off()
    
    png(paste(plot_folder,"Quantiles_layer",layer,".png",sep = ""), height = 10, width = 10, units = "cm", res = 400)
    testQuantiles(sim1_negBinomial) # residual plot equipped with regression lines that are either solid (actual quantiles) or dashed (theoretical quantiles)
    dev.off()
    
    # NOTE: following package recommendations, the default test of dispersion is 
    # used with *conditional* simulations (i.e., conditional on random effect 
    # structure)
    sim1_negBinomial_conditional <- simulateResiduals(fittedModel = m,
                                                      n = 250, plot = F, use.u = T)
    x <- testDispersion(sim1_negBinomial_conditional) # over/underdispersion
    dispersion <- append(dispersion,x$statistic)
    
  }
})

D = data.frame(matrix(cbind(unlist(zeroinflation),unlist(dispersion)),nrow = 40))
rownames(D) <- paste("layer",seq(1,40))
colnames(D) <- c("zero_inflation","dispersion")
save(D, file = paste(model_folder,"assumptions_llama_original5.rda", sep = ""))


# ------------------------------------------------------------------------------
# (2.4) Llama-2-13b - cwe averaged over 2 sentences

plot_folder = ".../decontextualized/models_assumptions_plots/llama_original_2/"
model_folder = ".../decontextualized/results_llama_original_2/"
zeroinflation <- dispersion <- list()

suppressMessages({
  for (layer in 1:40){
    print(paste("layer",layer))
    
    # load model 
    load(paste(model_folder,"negBinomial_llama_original2_L",layer,".rda",sep = ""))
    
    # simulate residuals (default unconditional simulation: simulate across all 
    # hierarchical levels)
    set.seed(5)
    sim1_negBinomial <- simulateResiduals(fittedModel = m,
                                          n = 250, plot = F)
    
    x <- testZeroInflation(sim1_negBinomial) # zero inflation
    zeroinflation <- append(zeroinflation,x$statistic)
    
    png(paste(plot_folder,"QQplot_layer",layer,".png",sep = ""), height = 10, width = 10, units = "cm", res = 400)
    plotQQunif(sim1_negBinomial) # QQ-plot residuals
    dev.off()
    
    png(paste(plot_folder,"Quantiles_layer",layer,".png",sep = ""), height = 10, width = 10, units = "cm", res = 400)
    testQuantiles(sim1_negBinomial) # residual plot equipped with regression lines that are either solid (actual quantiles) or dashed (theoretical quantiles)
    dev.off()
    
    # NOTE: following package recommendations, the default test of dispersion is 
    # used with *conditional* simulations (i.e., conditional on random effect 
    # structure)
    sim1_negBinomial_conditional <- simulateResiduals(fittedModel = m,
                                                      n = 250, plot = F, use.u = T)
    x <- testDispersion(sim1_negBinomial_conditional) # over/underdispersion
    dispersion <- append(dispersion,x$statistic)
    
  }
})

D = data.frame(matrix(cbind(unlist(zeroinflation),unlist(dispersion)),nrow = 40))
rownames(D) <- paste("layer",seq(1,40))
colnames(D) <- c("zero_inflation","dispersion")
save(D, file = paste(model_folder,"assumptions_llama_original2.rda", sep = ""))


# ------------------------------------------------------------------------------
# (2.5) Llama-2-13b - alternative wording

plot_folder = ".../decontextualized/models_assumptions_plots/llama_max/"
model_folder = ".../decontextualized/results_llama_max/"
zeroinflation <- dispersion <- list()

suppressMessages({
  for (layer in 1:40){
    print(paste("layer",layer))
    
    # load model 
    load(paste(model_folder,"negBinomial_llama_max_L",layer,".rda",sep = ""))
    
    # simulate residuals (default unconditional simulation: simulate across all 
    # hierarchical levels)
    set.seed(5)
    sim1_negBinomial <- simulateResiduals(fittedModel = m,
                                          n = 250, plot = F)
    
    x <- testZeroInflation(sim1_negBinomial) # zero inflation
    zeroinflation <- append(zeroinflation,x$statistic)
    
    png(paste(plot_folder,"QQplot_layer",layer,".png",sep = ""), height = 10, width = 10, units = "cm", res = 400)
    plotQQunif(sim1_negBinomial) # QQ-plot residuals
    dev.off()
    
    png(paste(plot_folder,"Quantiles_layer",layer,".png",sep = ""), height = 10, width = 10, units = "cm", res = 400)
    testQuantiles(sim1_negBinomial) # residual plot equipped with regression lines that are either solid (actual quantiles) or dashed (theoretical quantiles)
    dev.off()
    
    # NOTE: following package recommendations, the default test of dispersion is 
    # used with *conditional* simulations (i.e., conditional on random effect 
    # structure)
    sim1_negBinomial_conditional <- simulateResiduals(fittedModel = m,
                                                      n = 250, plot = F, use.u = T)
    x <- testDispersion(sim1_negBinomial_conditional) # over/underdispersion
    dispersion <- append(dispersion,x$statistic)
    
  }
})

D = data.frame(matrix(cbind(unlist(zeroinflation),unlist(dispersion)),nrow = 40))
rownames(D) <- paste("layer",seq(1,40))
colnames(D) <- c("zero_inflation","dispersion")
save(D, file = paste(model_folder,"assumptions_llama_max.rda", sep = ""))

