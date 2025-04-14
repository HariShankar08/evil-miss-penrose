
### LOADING PACKAGES
if(!require("pacman")) install.packages("pacman")
library("pacman")
p_load("dplyr","lme4","ggplot2","readr","readxl","lmerTest","reshape2", "MASS",
       "Rcpp","philentropy","DHARMa","corrplot")



############### (0) DATA PREPARATION -------------------------------------------

item_freqs <- read.csv(".../decontextualized/UkWac_items_long.csv")
item_freqs$study_source <- factor(item_freqs$study_source)

# ----- plot dependent variable
DV_plot <- data.frame(item_freqs$judgement)
colnames(DV_plot)[1] <- "rating_frequency"
DV_plot %>%
  ggplot(aes(x = rating_frequency)) +
  geom_histogram(color = "black", fill = "grey92", lwd = 0.25, bins = 40) +
  labs(x = "selection frequency") +
  theme_classic() +
  ggtitle("Frequency distribution of possible relations task judgements") +
  theme(plot.title = element_text(size = 10))

# standardize predictors
item_freqs <- item_freqs %>%
  mutate_at(c(8:ncol(item_freqs)), 
            ~(scale(.) %>% 
                as.vector))



############### (1) COUNT-BASED REGRESSION MODELS 

# run negative binomial regression model with random effects for methods 
# "original relations" and "alternative wording", and for the baseline models

set.seed(5)




# ______________________________________________________________________________
# ____________________________ (0) BASELINE MODELS _____________________________


# store models AIC, coefficients and p-values
aics <- betas <- alphas <- list() 

bas_vecDiffs <- glmer.nb(judgement ~ baseline_vecDiffs # model 1: relations as vector differences
                         + study_source + (1|relation),
                         data = item_freqs, na.action = na.omit)
betas <- append(betas,summary(bas_vecDiffs)$coefficients[,1])
alphas <- append(alphas,summary(bas_vecDiffs)$coefficients[,4])
aics <- append(aics,AIC(bas_vecDiffs))
save(bas_vecDiffs,file = "baseline_vecDiffs.rda")

bas_vecs <- glmer.nb(judgement ~ baseline_vecs # model 2: relations as vectors
                     + study_source + (1|relation),
                     data = item_freqs, na.action = na.omit)
betas <- append(betas,summary(bas_vecs)$coefficients[,1])
alphas <- append(alphas,summary(bas_vecs)$coefficients[,4])
aics <- append(aics,AIC(bas_vecs))
save(bas_vecs,file = "baseline_vecs.rda")

bas_vecs_CAOSS <- glmer.nb(judgement ~ baseline_vecs_CAOSS # model 3: relations as vectors using CAOSS model
                     + study_source + (1|relation),
                     data = item_freqs, na.action = na.omit)
betas <- append(betas,summary(bas_vecs_CAOSS)$coefficients[,1])
alphas <- append(alphas,summary(bas_vecs_CAOSS)$coefficients[,4])
aics <- append(aics,AIC(bas_vecs_CAOSS))
save(bas_vecs_CAOSS,file = "baseline_vecs_CAOSS.rda")

# export dataframe of model coefficients, significance values and AIC
d <- c(rbind(unlist(betas),unlist(alphas)))
res_bas <- cbind(data.frame(matrix(d, nrow = 3, byrow = TRUE)),data.frame(matrix(unlist(aics), nrow = 3)))
rownames(res_bas) <- c("baseline_vecDiffs","baseline_vecs","baseline_vecs_CAOSS")
colnames(res_bas) <- c("intercept","alpha_intercept","beta","alpha_beta",
                       "study_source","alpha_study_source","AIC")
save(res_bas, file = "modelsSummaries_baseline.rda")




# ______________________________________________________________________________
# _______________________ (1) METHOD: ORGINAL RELATIONS ________________________


# ------------------------------------------------------------------------------
# (1.1) BERT-base 

# store models AIC, coefficients and p-values
aics <- betas <- alphas <- list() 

for (i in 1:12){
  
  # model 
  f <- formula(paste("judgement ~ p_BERT_original_L",i," + study_source + (1|relation)", sep = ""))
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
f <- formula(paste("judgement ~ p_BERT_original_L",i_fittest," + study_source + (1|relation)", sep = ""))
m_out <- glmer.nb(f,data = item_freqs_out,na.action = na.omit)
save(m_out,file = paste("negBinomial_BERT_original_L",i_fittest,"_out.rda",sep = "")) # export

# export dataframe of model coefficients, significance values and AIC
d <- c(rbind(unlist(betas),unlist(alphas)))
res <- cbind(data.frame(matrix(d, nrow = 12, byrow = TRUE)),data.frame(matrix(unlist(aics), nrow = 12)))
rownames(res) <- paste("layer",seq(1,12))
colnames(res) <- c("intercept","alpha_intercept","p_BERT_original",
                   "alpha_p_BERT_original","study_source","alpha_study_source","AIC")
save(res, file = "modelsSummaries_BERT_original.rda")



# ------------------------------------------------------------------------------
# (1.2) Llama-2-13b

# store models AIC, coefficients and p-values
aics <- betas <- alphas <- list() 

for (i in 1:40){
  
  # model 
  f <- formula(paste("judgement ~ p_llama_original_L",i," + study_source + (1|relation)", sep = ""))
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
f <- formula(paste("judgement ~ p_llama_original_L",i_fittest," + study_source + (1|relation)", sep = ""))
m_out <- glmer.nb(f,data = item_freqs_out,na.action = na.omit)
save(m_out,file = paste("negBinomial_llama_original_L",i_fittest,"_out.rda",sep = "")) # export

# export dataframe of model coefficients, significance values and AIC
d <- c(rbind(unlist(betas),unlist(alphas)))
res <- cbind(data.frame(matrix(d, nrow = 40, byrow = TRUE)),data.frame(matrix(unlist(aics), nrow = 40)))
rownames(res) <- paste("layer",seq(1,40))
colnames(res) <- c("intercept","alpha_intercept","p_llama_original",
                   "alpha_p_llama_original","study_source","alpha_study_source","AIC")
save(res, file = "modelsSummaries_llama_original.rda")




# ______________________________________________________________________________
# ____________________ (2) CONTROL: ALTERNATIVE WORDING ________________________

# repeat previous analyses with relation probabilities obtained via the 
# "alternative wording" method

# ------------------------------------------------------------------------------
# (2.1) BERT-base

# store models AIC, coefficients and p-values
aics <- betas <- alphas <- list() 

for (i in 1:12){
  
  # model 
  f <- formula(paste("judgement ~ p_BERT_max_L",i," + study_source + (1|relation)", sep = ""))
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
f <- formula(paste("judgement ~ p_BERT_max_L",i_fittest," + study_source + (1|relation)", sep = ""))
m_out <- glmer.nb(f,data = item_freqs_out,na.action = na.omit)
save(m_out,file = paste("negBinomial_BERT_max_L",i_fittest,"_out.rda",sep = "")) # export

# export dataframe of model coefficients, significance values and AIC
d <- c(rbind(unlist(betas),unlist(alphas)))
res <- cbind(data.frame(matrix(d, nrow = 12, byrow = TRUE)),data.frame(matrix(unlist(aics), nrow = 12)))
rownames(res) <- paste("layer",seq(1,12))
colnames(res) <- c("intercept","alpha_intercept","p_BERT_max",
                   "alpha_p_BERT_max","study_source","alpha_study_source","AIC")
save(res, file = "modelsSummaries_BERT_max.rda")


# ------------------------------------------------------------------------------
# (2.2) Llama-2-13b

# store models AIC, coefficients and p-values
aics <- betas <- alphas <- list() 

for (i in 1:40){
  
  # model 
  f <- formula(paste("judgement ~ p_llama_max_L",i," + study_source + (1|relation)", sep = ""))
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
f <- formula(paste("judgement ~ p_llama_max_L",i_fittest," + study_source + (1|relation)", sep = ""))
m_out <- glmer.nb(f,data = item_freqs_out,na.action = na.omit)
save(m_out,file = paste("negBinomial_llama_max_L",i_fittest,"_out.rda",sep = "")) # export

# export dataframe of model coefficients, significance values and AIC
d <- c(rbind(unlist(betas),unlist(alphas)))
res <- cbind(data.frame(matrix(d, nrow = 40, byrow = TRUE)),data.frame(matrix(unlist(aics), nrow = 40)))
rownames(res) <- paste("layer",seq(1,40))
colnames(res) <- c("intercept","alpha_intercept","p_llama_max",
                   "alpha_p_llama_max","study_source","alpha_study_source","AIC")
save(res, file = "modelsSummaries_llama_max.rda")




# ______________________________________________________________________________
# _________________ (3.1) CONTROL: ORIGINAL FROM 10 SENTENCES __________________

# repeat previous analyses with relation probabilities obtained via the 
# "original" method using embeddings averaged over 10 sentences

# ------------------------------------------------------------------------------
# (3.1.1) BERT-base

# store models AIC, coefficients and p-values
aics <- betas <- alphas <- list() 

for (i in 1:12){
  
  # model 
  f <- formula(paste("judgement ~ p_BERT_original10_L",i," + study_source + (1|relation)", sep = ""))
  m <- glmer.nb(f,data = item_freqs,na.action = na.omit)
  save(m,file = paste("negBinomial_BERT_original10_L",i,".rda",sep = "")) # export
  
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
f <- formula(paste("judgement ~ p_BERT_original10_L",i_fittest," + study_source + (1|relation)", sep = ""))
m_out <- glmer.nb(f,data = item_freqs_out,na.action = na.omit)
save(m_out,file = paste("negBinomial_BERT_original10_L",i_fittest,"_out.rda",sep = "")) # export

# export dataframe of model coefficients, significance values and AIC
d <- c(rbind(unlist(betas),unlist(alphas)))
res <- cbind(data.frame(matrix(d, nrow = 12, byrow = TRUE)),data.frame(matrix(unlist(aics), nrow = 12)))
rownames(res) <- paste("layer",seq(1,12))
colnames(res) <- c("intercept","alpha_intercept","p_BERT_original10",
                   "alpha_p_BERT_original10","study_source","alpha_study_source","AIC")
save(res, file = "modelsSummaries_BERT_original10.rda")


# ------------------------------------------------------------------------------
# (3.1.2) Llama-2-13b

# store models AIC, coefficients and p-values
aics <- betas <- alphas <- list() 

for (i in 1:40){
  
  # model 
  f <- formula(paste("judgement ~ p_llama_original10_L",i," + study_source + (1|relation)", sep = ""))
  m <- glmer.nb(f,data = item_freqs,na.action = na.omit)
  save(m,file = paste("negBinomial_llama_original10_L",i,".rda",sep = "")) # export
  
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
f <- formula(paste("judgement ~ p_llama_original10_L",i_fittest," + study_source + (1|relation)", sep = ""))
m_out <- glmer.nb(f,data = item_freqs_out,na.action = na.omit)
save(m_out,file = paste("negBinomial_llama_original10_L",i_fittest,"_out.rda",sep = "")) # export

# export dataframe of model coefficients, significance values and AIC
d <- c(rbind(unlist(betas),unlist(alphas)))
res <- cbind(data.frame(matrix(d, nrow = 40, byrow = TRUE)),data.frame(matrix(unlist(aics), nrow = 40)))
rownames(res) <- paste("layer",seq(1,40))
colnames(res) <- c("intercept","alpha_intercept","p_llama_original10",
                   "alpha_p_llama_original10","study_source","alpha_study_source","AIC")
save(res, file = "modelsSummaries_llama_original10.rda")




# ______________________________________________________________________________
# _________________ (3.2) CONTROL: ORIGINAL FROM 5 SENTENCES ___________________

# repeat previous analyses with relation probabilities obtained via the 
# "original" method using embeddings averaged over 5 sentences

# ------------------------------------------------------------------------------
# (3.2.1) BERT-base

# store models AIC, coefficients and p-values
aics <- betas <- alphas <- list() 

for (i in 1:12){
  
  # model 
  f <- formula(paste("judgement ~ p_BERT_original5_L",i," + study_source + (1|relation)", sep = ""))
  m <- glmer.nb(f,data = item_freqs,na.action = na.omit)
  save(m,file = paste("negBinomial_BERT_original5_L",i,".rda",sep = "")) # export
  
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
f <- formula(paste("judgement ~ p_BERT_original5_L",i_fittest," + study_source + (1|relation)", sep = ""))
m_out <- glmer.nb(f,data = item_freqs_out,na.action = na.omit)
save(m_out,file = paste("negBinomial_BERT_original5_L",i_fittest,"_out.rda",sep = "")) # export

# export dataframe of model coefficients, significance values and AIC
d <- c(rbind(unlist(betas),unlist(alphas)))
res <- cbind(data.frame(matrix(d, nrow = 12, byrow = TRUE)),data.frame(matrix(unlist(aics), nrow = 12)))
rownames(res) <- paste("layer",seq(1,12))
colnames(res) <- c("intercept","alpha_intercept","p_BERT_original5",
                   "alpha_p_BERT_original5","study_source","alpha_study_source","AIC")
save(res, file = "modelsSummaries_BERT_original5.rda")


# ------------------------------------------------------------------------------
# (3.2.2) Llama-2-13b

# store models AIC, coefficients and p-values
aics <- betas <- alphas <- list() 

for (i in 1:40){
  
  # model 
  f <- formula(paste("judgement ~ p_llama_original5_L",i," + study_source + (1|relation)", sep = ""))
  m <- glmer.nb(f,data = item_freqs,na.action = na.omit)
  save(m,file = paste("negBinomial_llama_original5_L",i,".rda",sep = "")) # export
  
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
f <- formula(paste("judgement ~ p_llama_original5_L",i_fittest," + study_source + (1|relation)", sep = ""))
m_out <- glmer.nb(f,data = item_freqs_out,na.action = na.omit)
save(m_out,file = paste("negBinomial_llama_original5_L",i_fittest,"_out.rda",sep = "")) # export

# export dataframe of model coefficients, significance values and AIC
d <- c(rbind(unlist(betas),unlist(alphas)))
res <- cbind(data.frame(matrix(d, nrow = 40, byrow = TRUE)),data.frame(matrix(unlist(aics), nrow = 40)))
rownames(res) <- paste("layer",seq(1,40))
colnames(res) <- c("intercept","alpha_intercept","p_llama_original5",
                   "alpha_p_llama_original5","study_source","alpha_study_source","AIC")
save(res, file = "modelsSummaries_llama_original5.rda")




# ______________________________________________________________________________
# _________________ (3.3) CONTROL: ORIGINAL FROM 2 SENTENCES ___________________

# repeat previous analyses with relation probabilities obtained via the 
# "original" method using embeddings averaged over 2 sentences

# ------------------------------------------------------------------------------
# (3.3.1) BERT-base

# store models AIC, coefficients and p-values
aics <- betas <- alphas <- list() 

for (i in 1:12){
  
  # model 
  f <- formula(paste("judgement ~ p_BERT_original2_L",i," + study_source + (1|relation)", sep = ""))
  m <- glmer.nb(f,data = item_freqs,na.action = na.omit)
  save(m,file = paste("negBinomial_BERT_original2_L",i,".rda",sep = "")) # export
  
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
f <- formula(paste("judgement ~ p_BERT_original2_L",i_fittest," + study_source + (1|relation)", sep = ""))
m_out <- glmer.nb(f,data = item_freqs_out,na.action = na.omit)
save(m_out,file = paste("negBinomial_BERT_original2_L",i_fittest,"_out.rda",sep = "")) # export

# export dataframe of model coefficients, significance values and AIC
d <- c(rbind(unlist(betas),unlist(alphas)))
res <- cbind(data.frame(matrix(d, nrow = 12, byrow = TRUE)),data.frame(matrix(unlist(aics), nrow = 12)))
rownames(res) <- paste("layer",seq(1,12))
colnames(res) <- c("intercept","alpha_intercept","p_BERT_original2",
                   "alpha_p_BERT_original2","study_source","alpha_study_source","AIC")
save(res, file = "modelsSummaries_BERT_original2.rda")


# ------------------------------------------------------------------------------
# (3.3.2) Llama-2-13b

# store models AIC, coefficients and p-values
aics <- betas <- alphas <- list() 

for (i in 1:40){
  
  # model 
  f <- formula(paste("judgement ~ p_llama_original2_L",i," + study_source + (1|relation)", sep = ""))
  m <- glmer.nb(f,data = item_freqs,na.action = na.omit)
  save(m,file = paste("negBinomial_llama_original2_L",i,".rda",sep = "")) # export
  
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
f <- formula(paste("judgement ~ p_llama_original2_L",i_fittest," + study_source + (1|relation)", sep = ""))
m_out <- glmer.nb(f,data = item_freqs_out,na.action = na.omit)
save(m_out,file = paste("negBinomial_llama_original2_L",i_fittest,"_out.rda",sep = "")) # export

# export dataframe of model coefficients, significance values and AIC
d <- c(rbind(unlist(betas),unlist(alphas)))
res <- cbind(data.frame(matrix(d, nrow = 40, byrow = TRUE)),data.frame(matrix(unlist(aics), nrow = 40)))
rownames(res) <- paste("layer",seq(1,40))
colnames(res) <- c("intercept","alpha_intercept","p_llama_original2",
                   "alpha_p_llama_original2","study_source","alpha_study_source","AIC")
save(res, file = "modelsSummaries_llama_original2.rda")



