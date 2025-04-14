

# LOADING PACKAGES
if(!require("pacman")) install.packages("pacman")
library("pacman")
p_load("dplyr","lme4","ggplot2","readr","readxl","lmerTest","optimx","psych",
       "corrplot","DHARMa")


# ______________________________________________________________________________
# _______________________________ (1) PLOTTING _________________________________


###### plot by-layer coefficients

plot_folder <- ".../decontextualized/model_plots"

load(".../decontextualized/results_baseline_models/modelsSummaries_baseline.rda")
load(".../decontextualized/results_BERT_max/modelsSummaries_BERT_max.rda")
bert_max <- res
load(".../decontextualized/results_BERT_original/modelsSummaries_BERT_original.rda")
bert <- res
load(".../decontextualized/results_BERT_original10/modelsSummaries_BERT_original10.rda")
bert10 <- res
load(".../decontextualized/results_BERT_original5/modelsSummaries_BERT_original5.rda")
bert5 <- res
load(".../decontextualized/results_BERT_original2/modelsSummaries_BERT_original2.rda")
bert2 <- res
load(".../decontextualized/results_llama_max/modelsSummaries_llama_max.rda")
llama_max <- res
load(".../decontextualized/results_llama_original/modelsSummaries_llama_original.rda")
llama <- res
load(".../decontextualized/results_llama_original10/modelsSummaries_llama_original10.rda")
llama10 <- res
load(".../decontextualized/results_llama_original5/modelsSummaries_llama_original5.rda")
llama5 <- res
load(".../decontextualized/results_llama_original2/modelsSummaries_llama_original2.rda")
llama2 <- res
rm(res)


# ------------------------------------------------------------------------------
# (1.1) coefficients 

png(paste(plot_folder,"/UkWac_byLayer.png",sep = ""), height = 16, width = 24, units = "cm", res = 1000)

top <- max(c(res_bas[,3],bert[,3],bert_max[,3],llama[,3],llama_max[,3]))
bottom <- min(c(res_bas[,3],bert[,3],bert_max[,3],llama[,3],llama_max[,3]))

plot(rep(res_bas$beta[1],40), pch = 21, col = "darkslategray4", type = "o", lwd = 1.5,
     bg = ifelse(res_bas$alpha_beta[1] < 0.05, "darkslategray4","white"),
     xlab = "layer", ylab = "coefficient", xaxt = "n", yaxt = "n",
     main = "Decontextualized existing compounds: by-layer predictability",
     xlim = c(1,40), ylim = c(bottom-0.01,top+0.01))
points(rep(res_bas$beta[2],40), pch = 21, col = "chartreuse3", type = "o", lwd = 1.5,
       bg = ifelse(res_bas$alpha_beta[2] < 0.05, "chartreuse3","white"))
points(rep(res_bas$beta[3],40), pch = 21, col = "darkgreen", type = "o", lwd = 1.5,
       bg = ifelse(res_bas$alpha_beta[3] < 0.05, "darkgreen","white"))


points(bert_max$p_BERT_max, pch = 23, col = rgb(0.15,0.25,0.55, alpha = 0.7), type = "o", lwd = 1,
       bg = ifelse(bert_max$alpha_p_BERT_max*12 < 0.05, rgb(0.15,0.25,0.55, alpha = 0.7),"white"))
points(bert$p_BERT_original, pch = 21, col = "royalblue4", type = "o", lwd = 1.5,
       bg = ifelse(bert$alpha_p_BERT_original*12 < 0.05, "royalblue4","white"))
points(bert10$p_BERT_original10, pch = 21, col = "royalblue3", type = "o", lwd = 1.5,
       bg = ifelse(bert10$alpha_p_BERT_original10*12 < 0.05, "royalblue3","white"))
points(bert5$p_BERT_original5, pch = 21, col = "royalblue2", type = "o", lwd = 1.5,
       bg = ifelse(bert5$alpha_p_BERT_original5*12 < 0.05, "royalblue2","white"))
points(bert2$p_BERT_original2, pch = 21, col = "royalblue1", type = "o", lwd = 1.5,
       bg = ifelse(bert2$alpha_p_BERT_original2*12 < 0.05, "royalblue1","white"))

points(llama_max$p_llama_max, pch = 23, col = rgb(0.55,0,0, alpha = 0.4), type = "o", lwd = 1,
       bg = ifelse(llama_max$alpha_p_llama_max*12 < 0.05, rgb(0.55,0,0, alpha = 0.4),"white"))
points(llama$p_llama_original, pch = 21, col = "red4", type = "o", lwd = 1.5,
       bg = ifelse(llama$alpha_p_llama_original*12 < 0.05, "red4","white"))
points(llama10$p_llama_original10, pch = 21, col = "red3", type = "o", lwd = 1.5,
       bg = ifelse(llama10$alpha_p_llama_original10*12 < 0.05, "red3","white"))
points(llama5$p_llama_original5, pch = 21, col = "red2", type = "o", lwd = 1.5,
       bg = ifelse(llama5$alpha_p_llama_original5*12 < 0.05, "red2","white"))
points(llama2$p_llama_original2, pch = 21, col = "red1", type = "o", lwd = 1.5,
       bg = ifelse(llama2$alpha_p_llama_original2*12 < 0.05, "red1","white"))

abline(h = c(round(seq(-0.15,0.65, by = 0.05),2)), col = rgb(0,0,0,alpha = 0.05))
abline(v = c(0:41), col = rgb(0,0,0,alpha = 0.05))

axis(side = 1, at = seq(1,40, by = 3), cex.axis = 0.8)
axis(side = 2, at = round(seq(-0.05,0.50, by = 0.05),2), cex.axis = 0.8)

legend("topright", c("BERT: original relations (15 sentences)",
                     "BERT: original relations (10 sentences)",
                     "BERT: original relations (5 sentences)",
                     "BERT: original relations (2 sentences)",
                     "BERT: alternative wording",
                     "Llama: original relations (15 sentences)",
                     "Llama: original relations (10 sentences)",
                     "Llama: original relations (5 sentences)",
                     "Llama: original relations (2 sentences)",
                     "Llama: alternative wording",
                     "baseline: vector difference",
                     "baseline: additive model",
                     "baseline: CAOSS model"),
       col = c("royalblue4","royalblue3","royalblue2","royalblue1",rgb(0.15,0.25,0.55, alpha = 0.7),"red4","red3","red2","red1",rgb(0.55,0,0, alpha = 0.4),"darkslategray4","chartreuse3","darkgreen"),
       pt.bg = c("royalblue4","royalblue3","royalblue2","royalblue1",rgb(0.15,0.25,0.55, alpha = 0.7),"red4","red3","red2","red1",rgb(0.55,0,0, alpha = 0.4),"darkslategray4","chartreuse3","darkgreen"),
       lwd = c(1.5,1.5,1.5,1.5,1,1.5,1.5,1.5,1.5,1,1.5,1.5,1.5),
       pch = c(21,21,21,21,23,21,21,21,21,23,21,21,21),
       cex = 0.8, bty = "n",
       y.intersp = 0.85)

dev.off()



# ------------------------------------------------------------------------------
# (1.2) model fit (AIC) 

png(paste(plot_folder,"/UkWac_byLayer_AIC.png",sep = ""), height = 16, width = 24, units = "cm", res = 1000)

top <- max(c(res_bas[,7],bert[,7],bert_max[,7],llama[,7],llama_max[,7]))
bottom <- min(c(res_bas[,7],bert[,7],bert_max[,7],llama[,7],llama_max[,7]))

plot(rep(res_bas$AIC[1],40), col = "darkslategray4", type = "o", lwd = 1.5,
     xlab = "layer", ylab = "AIC", xaxt = "n", yaxt = "n", pch = 20, cex = 0.6,
     main = "Decontextualized existing compounds: by-layer model fit",
     xlim = c(1,40), ylim = c(bottom-5,top+5))
points(rep(res_bas$AIC[2],40), col = "chartreuse3", type = "o", pch = 20, cex = 0.6, lwd = 1.5)
points(rep(res_bas$AIC[3],40), col = "darkgreen", type = "o", pch = 20, cex = 0.6, lwd = 1.5)

points(bert_max$AIC, col = rgb(0.15,0.25,0.55, alpha = 0.7), type = "o", pch = 20, cex = 0.6, lwd = 1)
points(bert$AIC, col = "royalblue4", type = "o", pch = 20, cex = 0.6, lwd = 1.5)
points(bert10$AIC, col = "royalblue3", type = "o", pch = 20, cex = 0.6, lwd = 1.5)
points(bert5$AIC, col = "royalblue2", type = "o", pch = 20, cex = 0.6, lwd = 1.5)
points(bert2$AIC, col = "royalblue1", type = "o", pch = 20, cex = 0.6, lwd = 1.5)

points(llama_max$AIC, col = rgb(0.55,0,0, alpha = 0.4), type = "o", pch = 20, cex = 0.6, lwd = 1)
points(llama$AIC, col = "red4", type = "o", pch = 20, cex = 0.6, lwd = 1.5)
points(llama10$AIC, col = "red3", type = "o", pch = 20, cex = 0.6, lwd = 1.5)
points(llama5$AIC, col = "red2", type = "o", pch = 20, cex = 0.6, lwd = 1.5)
points(llama2$AIC, col = "red1", type = "o", pch = 20, cex = 0.6, lwd = 1.5)

abline(h = c(round(seq(63760,65320, by = 60),2)), col = rgb(0,0,0,alpha = 0.05))
abline(v = c(0:41), col = rgb(0,0,0,alpha = 0.05))

axis(side = 1, at = seq(1,40,by = 3), cex.axis = 0.8)
axis(side = 2, at = round(seq(63760,65320, by = 60),2), cex.axis = 0.8)
legend("bottomright", c("BERT: original relations (15 sentences)",
                     "BERT: original relations (10 sentences)",
                     "BERT: original relations (5 sentences)",
                     "BERT: original relations (2 sentences)",
                     "BERT: alternative wording",
                     "Llama: original relations (15 sentences)",
                     "Llama: original relations (10 sentences)",
                     "Llama: original relations (5 sentences)",
                     "Llama: original relations (2 sentences)",
                     "Llama: alternative wording",
                     "baseline: vector difference",
                     "baseline: additive model",
                     "baseline: CAOSS model"),
       col = c("royalblue4","royalblue3","royalblue2","royalblue1",rgb(0.15,0.25,0.55, alpha = 0.7),"red4","red3","red2","red1",rgb(0.55,0,0, alpha = 0.4),"darkslategray4","chartreuse3","darkgreen"),
       pt.bg = c("royalblue4","royalblue3","royalblue2","royalblue1",rgb(0.15,0.25,0.55, alpha = 0.7),"red4","red3","red2","red1",rgb(0.55,0,0, alpha = 0.4),"darkslategray4","chartreuse3","darkgreen"),
       lwd = c(1.5,1.5,1.5,1.5,1,1.5,1.5,1.5,1.5,1,1.5,1.5,1.5),
       pch = c(20,20,20,20,20,20,20,20,20,20,20,20,20),
       cex = 0.8, bty = "n",
       y.intersp = 0.85)
dev.off()




# ______________________________________________________________________________
# (2) plot results of top performing layer -----------------------------------

library(RColorBrewer)
nb.cols <- 16
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)

# ------------------------------------------------------------------------------
# BERT-base 

load(".../decontextualized/results_BERT_original/negBinomial_BERT_original_L12.rda") # load top-performing BERT-base layer

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
       x = "BERT-base relation probability, scaled (layer 12)")

ggsave(filename = "UkWac_multiPlot_BERT.jpeg", 
       path = plot_folder,
       width = 20, height = 15, units = "cm", device='jpeg', dpi=1000)
#ggsave(filename = "UkWac_singlePlot_BERT.jpeg", 
#       path = plot_folder,
#       width = 15, height = 15, units = "cm", device='jpeg', dpi=1000)

