
# LOADING PACKAGES
if(!require("pacman")) install.packages("pacman")
library("pacman")
p_load("dplyr","lme4","ggplot2","readr","readxl","lmerTest","optimx","psych",
       "corrplot","DHARMa")

# ______________________________________________________________________________
# _____________________ (1) MODEL ASSUMPTIONS: BERT-base _______________________

##### check negative Binomial models assumptions with DHARMa

# ------------------------------------------------------------------------------
# (1.1) BERT-base

plot_folder = ".../decontextualized/models_assumptions_plots/BERT_original/"
model_folder = ".../decontextualized/results_BERT_original/"
zeroinflation <- dispersion <- list()

suppressMessages({
  for (layer in 1:12){
    print(paste("layer",layer))
    
    # load model 
    load(paste(model_folder,"negBinomial_BERT_original_L",layer,".rda",sep = ""))
    
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

D = data.frame(matrix(cbind(unlist(zeroinflation),unlist(dispersion)),nrow = 12))
rownames(D) <- paste("layer",seq(1,12))
colnames(D) <- c("zero_inflation","dispersion")
save(D, file = paste(model_folder,"assumptions_BERT_original.rda", sep = ""))



# ------------------------------------------------------------------------------
# (1.2) BERT-base - cwe averaged over 10 sentences

plot_folder = ".../decontextualized/models_assumptions_plots/BERT_original_10/"
model_folder = ".../decontextualized/results_BERT_original_10/"
zeroinflation <- dispersion <- list()

suppressMessages({
  for (layer in 1:12){
    print(paste("layer",layer))
    
    # load model 
    load(paste(model_folder,"negBinomial_BERT_original10_L",layer,".rda",sep = ""))
    
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

D = data.frame(matrix(cbind(unlist(zeroinflation),unlist(dispersion)),nrow = 12))
rownames(D) <- paste("layer",seq(1,12))
colnames(D) <- c("zero_inflation","dispersion")
save(D, file = paste(model_folder,"assumptions_BERT_original10.rda", sep = ""))


# ------------------------------------------------------------------------------
# (1.3) BERT-base - cwe averaged over 5 sentences

plot_folder = ".../decontextualized/models_assumptions_plots/BERT_original_5/"
model_folder = ".../decontextualized/results_BERT_original_5/"
zeroinflation <- dispersion <- list()

suppressMessages({
  for (layer in 1:12){
    print(paste("layer",layer))
    
    # load model 
    load(paste(model_folder,"negBinomial_BERT_original5_L",layer,".rda",sep = ""))
    
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

D = data.frame(matrix(cbind(unlist(zeroinflation),unlist(dispersion)),nrow = 12))
rownames(D) <- paste("layer",seq(1,12))
colnames(D) <- c("zero_inflation","dispersion")
save(D, file = paste(model_folder,"assumptions_BERT_original5.rda", sep = ""))


# ------------------------------------------------------------------------------
# (1.4) BERT-base - cwe averaged over 2 sentences

plot_folder = ".../decontextualized/models_assumptions_plots/BERT_original_2/"
model_folder = ".../decontextualized/results_BERT_original_2/"
zeroinflation <- dispersion <- list()

suppressMessages({
  for (layer in 1:12){
    print(paste("layer",layer))
    
    # load model 
    load(paste(model_folder,"negBinomial_BERT_original2_L",layer,".rda",sep = ""))
    
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

D = data.frame(matrix(cbind(unlist(zeroinflation),unlist(dispersion)),nrow = 12))
rownames(D) <- paste("layer",seq(1,12))
colnames(D) <- c("zero_inflation","dispersion")
save(D, file = paste(model_folder,"assumptions_BERT_original2.rda", sep = ""))


# ------------------------------------------------------------------------------
# (1.5) BERT-base - alternative wording

plot_folder = ".../decontextualized/models_assumptions_plots/BERT_max/"
model_folder = ".../decontextualized/results_BERT_max/"
zeroinflation <- dispersion <- list()

suppressMessages({
  for (layer in 1:12){
    print(paste("layer",layer))
    
    # load model 
    load(paste(model_folder,"negBinomial_BERT_max_L",layer,".rda",sep = ""))
    
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

D = data.frame(matrix(cbind(unlist(zeroinflation),unlist(dispersion)),nrow = 12))
rownames(D) <- paste("layer",seq(1,12))
colnames(D) <- c("zero_inflation","dispersion")
save(D, file = paste(model_folder,"assumptions_BERT_max.rda", sep = ""))

