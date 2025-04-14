
### LOADING PACKAGES
if(!require("pacman")) install.packages("pacman")
library("pacman")
p_load("dplyr","lme4","ggplot2","readr","readxl","lmerTest","reshape2", "MASS",
       "Rcpp","philentropy","corrplot")


############### (0) DATA PREPARATION -------------------------------------------

item_freqs <- read.csv(".../spr_novel_count/spr_novel_items_long.csv")
item_freqs$context <- factor(item_freqs$context)

# ----- plot dependent variable
DV_plot <- data.frame(item_freqs$judgement)
colnames(DV_plot)[1] <- "rating_frequency"
DV_plot %>%
  ggplot(aes(x = rating_frequency)) +
  geom_histogram(color = "black", fill = "grey92", lwd = 0.25, bins = 40) +
  labs(x = "number of selections") +
  theme_classic() +
  ggtitle("Frequency distribution of possible relations task judgements") +
  theme(plot.title = element_text(size = 10))

# ----- scale variables
item_freqs <- item_freqs %>%
  mutate_at(c(7:ncol(item_freqs)), 
            ~(scale(.) %>% 
                as.vector))



############### (1) COUNT-BASED REGRESSION MODELS 

# run negative binomial regression model with random effects for methods 
# "original relations" and "alternative wording". 

set.seed(5)

# ______________________________________________________________________________
# ____________________________ (0) BASELINE MODELS _____________________________

# store models AIC, coefficients and p-values
aics <- betas <- alphas <- list() 

# MODEL 1: relations as vector differences
bas_vecDiffs <- glmer.nb(judgement ~ baseline_vecDiffs
                         + (1|relation),
                         data = item_freqs, na.action = na.omit)
betas <- append(betas,summary(bas_vecDiffs)$coefficients[,1])
alphas <- append(alphas,summary(bas_vecDiffs)$coefficients[,4])
aics <- append(aics,summary(bas_vecDiffs)$AIC[1])
save(bas_vecDiffs,file = "baseline_vecDiffs.rda")

# MODEL 2: relations as vectors - additive model
bas_vecs <- glmer.nb(judgement ~ baseline_vecs
                     + (1|relation),
                     data = item_freqs, na.action = na.omit)
betas <- append(betas,summary(bas_vecs)$coefficients[,1])
alphas <- append(alphas,summary(bas_vecs)$coefficients[,4])
aics <- append(aics,summary(bas_vecs)$AIC[1])
save(bas_vecs,file = "baseline_vecs.rda")

# MODEL 3: relations as vector - CAOSS model
bas_vecs_CAOSS <- glmer.nb(judgement ~ baseline_CAOSS
                           + (1|relation),
                           data = item_freqs, na.action = na.omit)
betas <- append(betas,summary(bas_vecs_CAOSS)$coefficients[,1])
alphas <- append(alphas,summary(bas_vecs_CAOSS)$coefficients[,4])
aics <- append(aics,summary(bas_vecs_CAOSS)$AIC[1])
save(bas_vecs_CAOSS,file = "baseline_vecs_CAOSS.rda")

# export dataframe of model coefficients, significance values and AIC
d <- c(rbind(unlist(betas),unlist(alphas)))
res_bas <- cbind(data.frame(matrix(d, nrow = 3, byrow = TRUE)),data.frame(matrix(unlist(aics), nrow = 3)))
rownames(res_bas) <- c("baseline_vecDiffs","baseline_vecs","baseline_vecs_CAOSS")
colnames(res_bas) <- c("intercept","alpha_intercept","beta","alpha_beta","AIC")
save(res_bas, file = "modelsSummaries_baseline.rda")



# ______________________________________________________________________________
# _______________________ (1) METHOD: ORGINAL RELATIONS ________________________


# ------------------------------------------------------------------------------
# (1.1) BERT-base 

# store models AIC, coefficients and p-values after model criticism
aics <- betas <- alphas <- list()

for (i in 1:12){
  
  # model 
  f <- formula(paste("judgement ~ p_BERT_original_L",i," + (1|relation)", sep = ""))
  m <- glmer.nb(f,data = item_freqs,na.action = na.omit)
  save(m,file = paste("negBinomial_BERT_original_L",i,".rda",sep = "")) # export
  
  if (all(AIC(m) < aics)){
    m_fittest <- m # select layer with lowest AIC
    i_fittest <- i
  }
  
  betas <- append(betas,summary(m)$coefficients[,1])
  alphas <- append(alphas,summary(m)$coefficients[,4])
  aics <- append(aics,summary(m)$AIC[1])
  
}

# model criticism (of best-performing layer)
item_freqs_out <- item_freqs[subset(abs(scale(resid(m_fittest)))) < 2.5,]
f <- formula(paste("judgement ~ p_BERT_original_L",i_fittest," + (1|relation)", sep = ""))
m_out <- glmer.nb(f,data = item_freqs_out,na.action = na.omit)
save(m_out,file = paste("negBinomial_BERT_original_L",i_fittest,"_out.rda",sep = "")) # export

# export dataframe of model coefficients, significance values and AIC
d <- c(rbind(unlist(betas),unlist(alphas)))
res <- cbind(data.frame(matrix(d, nrow = 12, byrow = TRUE)),data.frame(matrix(unlist(aics), nrow = 12)))
rownames(res) <- paste("layer",seq(1,12))
colnames(res) <- c("intercept","alpha_intercept","prob_BERT_original",
                   "alpha_prob_BERT_original","AIC")
save(res, file = "modelsSummaries_BERT_original.rda")


# ------------------------------------------------------------------------------
# (1.2) LLaMA-2-13B

# store models AIC, coefficients and p-values after model criticism
aics <- betas <- alphas <- list()

for (i in 1:40){
  
  # model 
  f <- formula(paste("judgement ~ p_llama_original_L",i," + (1|relation)", sep = ""))
  m <- glmer.nb(f,data = item_freqs,na.action = na.omit)
  save(m,file = paste("negBinomial_llama_original_L",i,".rda",sep = "")) # export
  
  if (all(AIC(m) < aics)){
    m_fittest <- m # select layer with lowest AIC
    i_fittest <- i
  }
  
  betas <- append(betas,summary(m)$coefficients[,1])
  alphas <- append(alphas,summary(m)$coefficients[,4])
  aics <- append(aics,summary(m)$AIC[1])
  
}

# model criticism (of best-performing layer)
item_freqs_out <- item_freqs[subset(abs(scale(resid(m_fittest)))) < 2.5,]
f <- formula(paste("judgement ~ p_llama_original_L",i_fittest," + (1|relation)", sep = ""))
m_out <- glmer.nb(f,data = item_freqs_out,na.action = na.omit)
save(m_out,file = paste("negBinomial_llama_original_L",i_fittest,"_out.rda",sep = "")) # export

# export dataframe of model coefficients, significance values and AIC
d <- c(rbind(unlist(betas),unlist(alphas)))
res <- cbind(data.frame(matrix(d, nrow = 40, byrow = TRUE)),data.frame(matrix(unlist(aics), nrow = 40)))
rownames(res) <- paste("layer",seq(1,40))
colnames(res) <- c("intercept","alpha_intercept","prob_llama_original",
                   "alpha_prob_llama_original","AIC")
save(res, file = "modelsSummaries_llama_original.rda")



# ______________________________________________________________________________
# ____________________ (2) CONTROL: ALTERNATIVE WORDING ________________________

# repeat previous analyses with relation probabilities obtained via the
# "alternative wording" method

# ------------------------------------------------------------------------------
# (2.1) BERT-base

# store models AIC, coefficients and p-values after model criticism
aics <- betas <- alphas <- list()

for (i in 1:12){
  
  # model 
  f <- formula(paste("judgement ~ p_BERT_max_L",i," + (1|relation)", sep = ""))
  m <- glmer.nb(f,data = item_freqs,na.action = na.omit)
  save(m,file = paste("negBinomial_BERT_max_L",i,".rda",sep = "")) # export
  
  if (all(AIC(m) < aics)){
    m_fittest <- m # select layer with lowest AIC
    i_fittest <- i
  }
  
  betas <- append(betas,summary(m)$coefficients[,1])
  alphas <- append(alphas,summary(m)$coefficients[,4])
  aics <- append(aics,summary(m)$AIC[1])
  
}

# model criticism (of best-performing layer)
item_freqs_out <- item_freqs[subset(abs(scale(resid(m_fittest)))) < 2.5,]
f <- formula(paste("judgement ~ p_BERT_max_L",i_fittest," + (1|relation)", sep = ""))
m_out <- glmer.nb(f,data = item_freqs_out,na.action = na.omit)
save(m_out,file = paste("negBinomial_BERT_max_L",i_fittest,"_out.rda",sep = "")) # export

# export dataframe of model coefficients, significance values and AIC
d <- c(rbind(unlist(betas),unlist(alphas)))
res <- cbind(data.frame(matrix(d, nrow = 12, byrow = TRUE)),data.frame(matrix(unlist(aics), nrow = 12)))
rownames(res) <- paste("layer",seq(1,12))
colnames(res) <- c("intercept","alpha_intercept","prob_BERT_max",
                   "alpha_prob_BERT_max","AIC")
save(res, file = "modelsSummaries_BERT_max.rda")


# ------------------------------------------------------------------------------
# (2.2) LLaMA-2-13B

# store models AIC, coefficients and p-values after model criticism
aics <- betas <- alphas <- list()

for (i in 1:40){
  
  # model 
  f <- formula(paste("judgement ~ p_llama_max_L",i," + (1|relation)", sep = ""))
  m <- glmer.nb(f,data = item_freqs,na.action = na.omit)
  save(m,file = paste("negBinomial_llama_max_L",i,".rda",sep = "")) # export
  
  if (all(AIC(m) < aics)){
    m_fittest <- m # select layer with lowest AIC
    i_fittest <- i
  }
  
  betas <- append(betas,summary(m)$coefficients[,1])
  alphas <- append(alphas,summary(m)$coefficients[,4])
  aics <- append(aics,summary(m)$AIC[1])
  
}

# model criticism (of best-performing layer)
item_freqs_out <- item_freqs[subset(abs(scale(resid(m_fittest)))) < 2.5,]
f <- formula(paste("judgement ~ p_llama_max_L",i_fittest," + (1|relation)", sep = ""))
m_out <- glmer.nb(f,data = item_freqs_out,na.action = na.omit)
save(m_out,file = paste("negBinomial_llama_max_L",i_fittest,"_out.rda",sep = "")) # export

# export dataframe of model coefficients, significance values and AIC
d <- c(rbind(unlist(betas),unlist(alphas)))
res <- cbind(data.frame(matrix(d, nrow = 40, byrow = TRUE)),data.frame(matrix(unlist(aics), nrow = 40)))
rownames(res) <- paste("layer",seq(1,40))
colnames(res) <- c("intercept","alpha_intercept","prob_llama_max",
                   "alpha_prob_llama_max","AIC")
save(res, file = "modelsSummaries_llama_max.rda")


