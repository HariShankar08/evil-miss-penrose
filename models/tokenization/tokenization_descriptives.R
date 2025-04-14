
### LOADING PACKAGES
if(!require("pacman")) install.packages("pacman")
library("pacman")
p_load("dplyr","lme4","ggplot2","readr","readxl","lmerTest","reshape2", "MASS",
       "Rcpp","philentropy","corrplot","cowplot")



############### TOKENIZATION DESCRIPTIVES ###################################### 

plot_folder = ".../tokenization_effect/"

# load tables of number of tokens for all compounds and datasets
tokens_decontextualized <- read.csv(".../tokens_decontextualized.csv")
tokens_existing <- read.csv(".../tokens_contextualized_existing.csv")
tokens_novel <- read.csv(".../tokens_contextualized_novel.csv")

png(paste(plot_folder,"N_token_counts.png",sep = ""), height = 12, width = 16, units = "cm", res = 1000)

h_1 <- ggplot(tokens_decontextualized, aes(x = N_tokens_BERT)) + 
  geom_histogram(binwidth = 0.5, fill = "royalblue4") +
  xlim(0.25,5.75) + ylim(0,700) +
  theme_classic() +
  theme(axis.title.x = element_blank()) +
  labs(x = "", y = "decontextualized\n")
h_2 <- ggplot(tokens_decontextualized, aes(x = N_tokens_Llama)) + 
  geom_histogram(binwidth = 0.5, fill = "red4") +
  xlim(0.25,5.75) + ylim(0,700) +
  theme_classic() +
  theme(axis.title.x = element_blank()) +
  labs(x = "", y = "")
h_3 <- ggplot(tokens_existing, aes(x = N_tokens_BERT)) + 
  geom_histogram(binwidth = 0.5, fill = "royalblue4") +
  xlim(0.25,5.75) + ylim(0,60) +
  theme_classic() +
  theme(axis.title.x = element_blank()) +
  labs(x = "", y = "contextualized\nexisting\n")
h_4 <- ggplot(tokens_existing, aes(x = N_tokens_Llama)) + 
  geom_histogram(binwidth = 0.5, fill = "red4") +
  xlim(0.25,5.75) + ylim(0,60) +
  theme_classic() +
  theme(axis.title.x = element_blank()) +
  labs(x = "", y = "")
h_5 <- ggplot(tokens_novel, aes(x = N_tokens_BERT)) + 
  geom_histogram(binwidth = 0.5, fill = "royalblue4") +
  xlim(0.25,5.75) + ylim(0,55) +
  theme_classic() + 
  labs(x = "BERT-base", y = "contextualized\nnovel\n")
h_6 <- ggplot(tokens_novel, aes(x = N_tokens_Llama)) + 
  geom_histogram(binwidth = 0.5, fill = "red4") +
  xlim(0.25,5.75) + ylim(0,55) +
  theme_classic() + 
  labs(x = "Llama-2-13b", y = "")

H <- plot_grid(h_1, h_2, h_3, h_4, h_5, h_6, ncol = 2, align = 'v', axis = 'l')
H <- add_sub(H, "number of tokens")
Ylab <- ggplot()+ geom_text(aes(x = 0,y = 0),label = "frequency count", 
                            size = 5 , angle = 90) + theme_void()
plot_grid(Ylab, H, nrow = 1, rel_widths = c(0.1,1, 0.1))

dev.off()



