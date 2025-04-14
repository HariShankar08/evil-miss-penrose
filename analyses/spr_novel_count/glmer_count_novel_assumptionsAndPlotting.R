
# LOADING PACKAGES
if(!require("pacman")) install.packages("pacman")
library("pacman")
p_load("dplyr","lme4","ggplot2","readr","readxl","lmerTest","optimx","psych",
       "corrplot","DHARMa")

# ______________________________________________________________________________
# ___________________________ (1) MODEL ASSUMPTIONS ____________________________

##### check negative Binomial models assumptions with DHARMa

# note: model_folder specifies the path to the folder where model results are saved (.rda files; glmer_count_computation.R outputs)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# (1.1) BERT-base

plot_folder = ".../spr_novel_count/models_assumptions_plots/plot_assumptions_BERT_original/" # path to the folder where plots are saved
model_folder = ".../spr_novel_count/results_BERT_original/"
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
# ------------------------------------------------------------------------------
# (1.2) LLaMA-2-13B

plot_folder = ".../spr_novel_count/models_assumptions_plots/plot_assumptions_llama_original/"
model_folder = ".../spr_novel_count/results_llama_original/"
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
# ------------------------------------------------------------------------------
# (1.3) baseline models

plot_folder = ".../spr_novel_count/models_assumptions_plots/plot_assumptions_baseline_models/"
model_folder = ".../spr_novel_count/results_baseline/"
models_names <- c("baseline_vecDiffs","baseline_vecs","baseline_vecs_CAOSS")

# load models
load(paste(model_folder,"/",models_names[1],".rda",sep = ""))
load(paste(model_folder,"/",models_names[2],".rda",sep = ""))
load(paste(model_folder,"/",models_names[3],".rda",sep = ""))

zeroinflation <- dispersion <- list()

# ------------------------------------------------------------------------------
# (1.3.1) vector difference

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
# (1.3.2) additive model

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
# (1.3.3) CAOSS model

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
# export summary

D = data.frame(matrix(cbind(unlist(zeroinflation),unlist(dispersion)),nrow = 3))
rownames(D) <- models_names
colnames(D) <- c("zero_inflation","dispersion")
save(D, file = paste(model_folder,"/assumptions_baseline_models.rda", sep = ""))




# ______________________________________________________________________________
# _______________________________ (2) PLOTTING _________________________________

plot_folder = ".../spr_novel_count/models_plots/"

# ______________________________________________________________________________
# (2.1) plot by-layer results - all models -------------------------------------

fold = ".../spr_novel_count"
load(paste(fold,"/results_baseline/modelsSummaries_baseline.rda",sep = ""))
load(paste(fold,"/results_BERT_original/modelsSummaries_BERT_original.rda",sep = ""))
res_BERT <- res
load(paste(fold,"/results_BERT_max/modelsSummaries_BERT_max.rda",sep = ""))
res_BERT_max <- res
load(paste(fold,"/results_llama_original/modelsSummaries_llama_original.rda",sep = ""))
res_llama <- res
load(paste(fold,"/results_llama_max/modelsSummaries_llama_max.rda",sep = ""))
res_llama_max <- res
rm(res)

# ------------------------------------------------------------------------------
# (2.1.1) coeffients 

png(paste(plot_folder,"spr_Novel_byLayer.png",sep = ""), height = 16, width = 24, units = "cm", res = 1000)

top <- max(c(res_bas[,3],res_BERT[,3],res_BERT_max[,3],res_llama[,3],res_llama_max[,3]))
bottom <- min(c(res_bas[,3],res_BERT[,3],res_BERT_max[,3],res_llama[,3],res_llama_max[,3]))

plot(rep(res_bas$beta[1],40), pch = 21, col = "darkslategray4", type = "o", lwd = 1.5,
     bg = ifelse(res_bas$alpha_beta[1] < 0.05, "darkslategray4","white"),
     xlab = "layer", ylab = "coefficient", xaxt = "n", yaxt = "n",
     main = "Contextualized novel compounds: by-layer predictability",
     xlim = c(1,40), ylim = c(bottom-0.01,top+0.01))
points(rep(res_bas$beta[2],40), pch = 21, col = "chartreuse3", type = "o", lwd = 1.5,
       bg = ifelse(res_bas$alpha_beta[2] < 0.05, "chartreuse3","white"))
points(rep(res_bas$beta[3],40), pch = 21, col = "darkgreen", type = "o", lwd = 1.5,
       bg = ifelse(res_bas$alpha_beta[3] < 0.05, "darkgreen","white"))

points(res_BERT_max$prob_BERT_max, pch = 23, col = rgb(0.15,0.25,0.55, alpha = 0.7), type = "o", lwd = 1,
       bg = ifelse(res_BERT_max$alpha_prob_BERT_max*12 < 0.05, rgb(0.15,0.25,0.55, alpha = 0.7),"white"))
points(res_BERT$prob_BERT_original, pch = 21, col = "royalblue4", type = "o", lwd = 1.5,
       bg = ifelse(res_BERT$alpha_prob_BERT_original*12 < 0.05, "royalblue4","white"))

points(res_llama_max$prob_llama_max, pch = 23, col = rgb(0.55,0,0, alpha = 0.4), 
       type = "o", lwd = 1,
       bg = ifelse(res_llama_max$alpha_prob_llama_max*40 < 0.05, rgb(0.55,0,0, alpha = 0.4),"white"))
points(res_llama$prob_llama_original, pch = 21, col = "red4", 
       type = "o", lwd = 1.5,
       bg = ifelse(res_llama$alpha_prob_llama_original*40 < 0.05, "red4","white"))

abline(h = c(round(seq(-0.15,0.25, by = 0.05),2)), col = rgb(0,0,0,alpha = 0.05))
abline(v = c(0:41), col = rgb(0,0,0,alpha = 0.05))

axis(side = 1, at = seq(1,40, by = 3), cex.axis = 0.8)
axis(side = 2, at = round(seq(-0.15,0.25, by = 0.05),2), cex.axis = 0.8)

legend("bottomleft", c("BERT: original relations",
                    "BERT: alternative wording",
                    "LLaMA-2-13B: original relations",
                    "LLaMA-2-13B: alternative wording",
                    "baseline: vector difference",
                    "baseline: additive model",
                    "baseline: CAOSS model"),
       col = c("royalblue4",rgb(0.15,0.25,0.55, alpha = 0.7),"red4",rgb(0.55,0,0, alpha = 0.4),"darkslategray4","chartreuse3","darkgreen"),
       pt.bg = c("royalblue4",rgb(0.15,0.25,0.55, alpha = 0.7),"red4",rgb(0.55,0,0, alpha = 0.4),"darkslategray4","chartreuse3","darkgreen"),
       lwd = c(1.5,1,1.5,1,1.5,1.5,1.5),
       pch = c(21,23,21,23,21,21,21),
       cex = 0.8, bty = "n",
       y.intersp = 0.85)
dev.off()


# ------------------------------------------------------------------------------
# (2.1.2) model fit (AIC) 

png(paste(plot_folder,"spr_Novel_byLayer_AIC.png",sep = ""), height = 16, width = 24, units = "cm", res = 1000)

top <- max(c(res_bas[,5],res_BERT[,5],res_BERT_max[,5],res_llama[,5],res_llama_max[,5]))
bottom <- min(c(res_bas[,5],res_BERT[,5],res_BERT_max[,5],res_llama[,5],res_llama_max[,5]))

plot(rep(res_bas$AIC[1],40), col = "darkslategray4", type = "o", lwd = 1.5,
     xlab = "layer", ylab = "AIC", xaxt = "n", yaxt = "n", pch = 20, cex = 0.6,
     main = "Contextualized novel compounds: by-layer model fit",
     xlim = c(1,40), ylim = c(bottom-5,top+5))
points(rep(res_bas$AIC[2],40), col = "chartreuse3", type = "o", pch = 20, cex = 0.6, lwd = 1.5)
points(rep(res_bas$AIC[3],40), col = "darkgreen", type = "o", pch = 20, cex = 0.6, lwd = 1.5)
points(res_BERT_max$AIC, col = rgb(0.15,0.25,0.55, alpha = 0.7), type = "o", pch = 20, cex = 0.6, lwd = 1)
points(res_BERT$AIC, col = "royalblue4", type = "o", pch = 20, cex = 0.6, lwd = 1.5)
points(res_llama_max$AIC, col = rgb(0.55,0,0, alpha = 0.4), type = "o", pch = 20, cex = 0.6, lwd = 1)
points(res_llama$AIC, col = "red4", type = "o", pch = 20, cex = 0.6, lwd = 1.5)

abline(h = c(round(seq(8740,8800, by = 10),2)), col = rgb(0,0,0,alpha = 0.05))
abline(v = c(0:41), col = rgb(0,0,0,alpha = 0.05))

axis(side = 1, at = seq(1,40,by = 3), cex.axis = 0.8)
axis(side = 2, at = round(seq(8740,8800, by = 10),2), cex.axis = 0.8)
legend("right", c("BERT: original relations",
                  "BERT: alternative wording",
                  "LLaMA-2-13B: original relations",
                  "LLaMA-2-13B: alternative wording",
                  "baseline: vector difference",
                  "baseline: additive model",
                  "baseline: CAOSS model"),
       col = c("royalblue4",rgb(0.15,0.25,0.55, alpha = 0.7),"red4",rgb(0.55,0,0, alpha = 0.4),"darkslategray4","chartreuse3","darkgreen"),
       pt.bg = c("royalblue4",rgb(0.15,0.25,0.55, alpha = 0.7),"red4",rgb(0.55,0,0, alpha = 0.4),"darkslategray4","chartreuse3","darkgreen"),
       lwd = c(1.5,1,1.5,1,1.5,1.5,1.5),
       pch = c(20,20,20,20,20,20,20),
       cex = 0.8, bty = "n",
       y.intersp = 0.85)
dev.off()


# ______________________________________________________________________________
# (2.2) plot results of top performing layer -----------------------------------

library(RColorBrewer)
nb.cols <- 16
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)

# ------------------------------------------------------------------------------
# (2.2.1) BERT-base 

load(".../spr_novel_count/results_BERT_original/negBinomial_BERT_original_L12.rda")

sjPlot::plot_model(m, type = "pred",
                   terms = c("p_BERT_original_L12", "relation"),
                   theme_classic(),
                   show.data = T,
                   pred.type = "re", # suppress to plot fixed main effect 
                   colors = mycolors[1:16],
                   ci.lvl = NA) +
  theme_classic() + 
  geom_line(lwd = 1, col = "grey12") +
  #geom_point(col = "grey12") +
  facet_wrap(~group) + # suppress to plot in one panel
  labs(y = "human selection frequency",
       x = "BERT-base relation probability (layer 12)")

ggsave(filename = "novel_multiPlot_BERT.jpeg", 
       path = plot_folder,
       width = 20, height = 15, units = "cm", device='jpeg', dpi=1000)
# single plot size: width = 15, height = 15


# ------------------------------------------------------------------------------
# (2.2.2) LLaMA-2-13B 

load(".../spr_novel_count/results_llama_original/negBinomial_llama_original_L16.rda")

sjPlot::plot_model(m, type = "pred",
                   terms = c("p_llama_original_L16", "relation"),
                   theme_classic(),
                   show.data = T,
                   pred.type = "re", # suppress to plot fixed main effect 
                   colors = mycolors[1:16],
                   ci.lvl = NA) +
  theme_classic() + 
  geom_line(lwd = 1, col = "grey12") +
  #geom_point(col = "grey12") +
  facet_wrap(~group) + # suppress to plot in one panel
  labs(y = "human selection frequency",
       x = "LLaMA-2-13B relation probability (layer 16)")

ggsave(filename = "novel_multiPlot_llama.jpeg", 
       path = plot_folder,
       width = 20, height = 15, units = "cm", device='jpeg', dpi=1000)
# single plot size: width = 15, height = 15


