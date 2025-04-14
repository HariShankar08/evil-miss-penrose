

# LOADING PACKAGES
if(!require("pacman")) install.packages("pacman")
library("pacman")
p_load("dplyr","lme4","ggplot2","readr","readxl","lmerTest","reshape2", "MASS",
       "Rcpp","philentropy","corrplot","cowplot","DHARMa")


# ______________________________________________________________________________
# _______________ get item examples based on prediction accuracy _______________

# get and plot best and worst predicted items (focusing on BERT-base layer 12)

# define basic desiderata
residual_type = "response" # which type of residual to extract
n_exp = 15 # what number of "good" and "bad" examples to extract 



# ______________________________________________________________________________
# (1) decontextualized

# note: model_folder specifies the path to the folder where model results are saved (.rda files; glmer_count_computation.r outputs)

# load data
model_folder = ".../UkWac_count/results_BERT_original/"
load(paste(model_folder,"negBinomial_BERT_original_L12.rda",sep = ""))

item_freqs <- read.csv(".../decontextualized/UkWac_items_long.csv") # path to csv input file

r <- residuals(m, type = residual_type)
r1 <- cbind(item_freqs, residuals = r)

cnames <- unique(r1$Compound)
rr <- rs <- c()
for(i in c(1:length(cnames))){
  
  rs <- rbind(rs,r1[grepl(cnames[i],r1$Compound),"residuals"])
  rr <- c(rr,mean(abs(r1[grepl(cnames[i],r1$Compound),"residuals"])))
}

rr1 <- data.frame(cnames,rr,rs)
colnames(rr1) <- gsub("_"," ",c("compound","deviation",r1[1:16,"relation"]))
rownames(rr1) <- rr1$compound
rr1 <- rr1[order(rr1$deviation), ]

rels <- colnames(rr1)[3:18]
long_data <- reshape(rr1[c(1:n_exp,(nrow(rr1)-(n_exp-1)):nrow(rr1)),], 
                     varying = rels,
                     v.names = "deviation", 
                     timevar = "relation", 
                     idvar = "compound", 
                     direction = "long")
long_data <- long_data %>%
  mutate(relation = rels[relation])
long_data$compound <- factor(long_data$compound, levels = rev(rownames(rr1)))
long_data$relation <- factor(long_data$relation, levels = rels)

h1 <- ggplot(long_data, aes(x = relation, y = compound, fill = deviation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue3", mid = "white", high = "red3", midpoint = 0, limit = c(min(long_data$deviation), max(long_data$deviation))) +
  theme_minimal() +
  labs(title = expression(atop("existing compounds rated ","in isolation (analysis 1)")), fill = "difference") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = n_exp + 0.5, color = "black", linewidth = 1) +
  theme_minimal() +
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom", legend.direction = "horizontal")



# ______________________________________________________________________________
# (2) contextualized existing

model_folder = ".../spr_existing_count/results_BERT_original/"
load(paste(model_folder,"negBinomial_BERT_original_L12.rda",sep = ""))

item_freqs <- read.csv(".../spr_existing_count/spr_existing_items_long.csv") # path to csv input file

r <- residuals(m, type = residual_type)
r2 <- cbind(item_freqs, residuals = r)

cnames <- unique(r2$sentence)
rr <- rs <- name <- c()
for(i in c(1:length(cnames))){
  name <- c(name,paste(r2[grepl(cnames[i],r2$sentence),"compound"][1],as.numeric(i%%2==0)+1))
  rs <- rbind(rs,r2[grepl(cnames[i],r2$sentence),"residuals"])
  rr <- c(rr,mean(abs(r2[grepl(cnames[i],r2$sentence),"residuals"])))
}

rr2 <- data.frame(name,rr,rs)
colnames(rr2) <- gsub("_"," ",c("compound","deviation",r1[1:16,"relation"]))
rownames(rr2) <- rr2$compound
rr2 <- rr2[order(rr2$deviation), ]

rels <- colnames(rr2)[3:18]
long_data <- reshape(rr2[c(1:n_exp,(nrow(rr2)-(n_exp-1)):nrow(rr2)),], 
                     varying = rels,
                     v.names = "deviation", 
                     timevar = "relation", 
                     idvar = "compound", 
                     direction = "long")
long_data <- long_data %>%
  mutate(relation = rels[relation])
long_data$compound <- factor(long_data$compound, levels = rev(rownames(rr2)))
long_data$relation <- factor(long_data$relation, levels = rels)

h2 <- ggplot(long_data, aes(x = relation, y = compound, fill = deviation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue3", mid = "white", high = "red3", midpoint = 0, limit = c(min(long_data$deviation), max(long_data$deviation))) +
  theme_minimal() +
  labs(title = expression(atop("existing compounds rated ","in context (analysis 2)")), fill = "difference") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = n_exp + 0.5, color = "black", linewidth = 1) +
  theme_minimal() +
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom", legend.direction = "horizontal")



# ______________________________________________________________________________
# (3) contextualized novel

model_folder = ".../spr_novel_count/results_BERT_original/"
load(paste(model_folder,"negBinomial_BERT_original_L12.rda",sep = ""))

item_freqs <- read.csv(".../spr_novel_count/spr_novel_items_long.csv") # path to csv input file

r <- residuals(m, type = residual_type)
r3 <- cbind(item_freqs, residuals = r)

cnames <- unique(r3$sentence)
rr <- rs <- name <- c()
for(i in c(1:length(cnames))){
  name <- c(name,paste(r3[grepl(cnames[i],r3$sentence),"compound"][1],as.numeric(i%%2==0)+1))
  rs <- rbind(rs,r3[grepl(cnames[i],r3$sentence),"residuals"])
  rr <- c(rr,mean(abs(r3[grepl(cnames[i],r3$sentence),"residuals"])))
}

rr3 <- data.frame(name,rr,rs)
colnames(rr3) <- gsub("_"," ",c("compound","deviation",r1[1:16,"relation"]))
rownames(rr3) <- rr3$compound
rr3 <- rr3[order(rr3$deviation), ]

rels <- colnames(rr3)[3:18]
long_data <- reshape(rr3[c(1:n_exp,(nrow(rr3)-(n_exp-1)):nrow(rr3)),], 
                     varying = rels,
                     v.names = "deviation", 
                     timevar = "relation", 
                     idvar = "compound", 
                     direction = "long")
long_data <- long_data %>%
  mutate(relation = rels[relation])
long_data$compound <- factor(long_data$compound, levels = rev(rownames(rr3)))
long_data$relation <- factor(long_data$relation, levels = rels)

h3 <- ggplot(long_data, aes(x = relation, y = compound, fill = deviation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue3", mid = "white", high = "red3", midpoint = 0, limit = c(min(long_data$deviation), max(long_data$deviation))) +
  theme_minimal() +
  labs(title = expression(atop("novel compounds rated ","in context (analysis 3)")), fill = "difference") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = n_exp + 0.5, color = "black", linewidth = 1) +
  theme_minimal() +
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom", legend.direction = "horizontal")


plot_folder = ".../analysis/plot/"
png(paste(plot_folder,"item_examples.png",sep = ""), height = 16, width = 26, units = "cm", res = 1200)
cowplot::plot_grid(h1,h2,h3,ncol = 3, align = "v")
dev.off()



