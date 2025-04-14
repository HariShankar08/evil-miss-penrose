
########## LOADING

## load packages
import torch
import random
import transformers
import csv
import numpy as np
from matplotlib import pyplot

# set output path
destination_folder = "...\\CWE_output\\tokenizer"

## import tokenizers 

# import BERT-base tokenizer
from transformers import BertTokenizer
tokenizer_bert = BertTokenizer.from_pretrained('bert-base-uncased')

# import Llama-2-13b tokenizer
from transformers import AutoTokenizer
tokenizer_llama = AutoTokenizer.from_pretrained("...\\llama_tokenizer")



########## TOKENIZING

## (1) -------------------- DECONTEXTUALIZED

# import sentences from decontextualized compounds
sentences = []
with open('...\\CWE\\sentence_variants.csv', newline='') as f:
    reader = csv.reader(f)
    sentences = list(reader)
compound_names = [row[1] for row in sentences][0:len(sentences):37*15]

# get number of tokens per compound
nTokens = []
nTokens.append(['compound','N_tokens_BERT','N_tokens_Llama'])

for i in range(0,len(compound_names)):
    n_tok_bert = len(tokenizer_bert.tokenize(compound_names[i])) # BERT
    n_tok_llama = len(tokenizer_llama.tokenize(compound_names[i])) # Llama-2-13b
    nTokens.append([compound_names[i],n_tok_bert,n_tok_llama])

# export
with open(destination_folder + '\\tokens_decontextualized.csv','w',newline='') as f:
    write = csv.writer(f)
    write.writerows(nTokens)

## -------------------- get examples
n_examples = 10
expTokens = []
expTokens.append(['compound','tokens_BERT','tokens_Llama'])

random.seed(1)
r_idx = random.sample(range(0,len(compound_names)), n_examples)

for i in range(0,n_examples):
    n_tok_bert = ', '.join(tokenizer_bert.tokenize(compound_names[r_idx[i]])) # BERT
    n_tok_llama = ', '.join(tokenizer_llama.tokenize(compound_names[r_idx[i]]))[1::] # Llama-2-13b (exclude first special character)
    expTokens.append([compound_names[r_idx[i]],n_tok_bert,n_tok_llama])

with open(destination_folder + '\\examples_decontextualized.csv','w',newline='',encoding="utf-8") as f:
    write = csv.writer(f)
    write.writerows(expTokens)



## (2) -------------------- EXISTING CONTEXTUALIZED

# import sentences from the self paced reading database (existing compounds)
sentences = []
with open('...\\CWE\\sentence_existing_spr.csv', newline='') as f:
    reader = csv.reader(f)
    sentences = list(reader)
compound_names = [row[1] for row in sentences][0:len(sentences):74]

# get number of tokens per compound
nTokens = []
nTokens.append(['compound','N_tokens_BERT','N_tokens_Llama'])

for i in range(0,len(compound_names)):
    n_tok_bert = len(tokenizer_bert.tokenize(compound_names[i])) # BERT
    n_tok_llama = len(tokenizer_llama.tokenize(compound_names[i])) # Llama-2-13b
    nTokens.append([compound_names[i],n_tok_bert,n_tok_llama])

# export
with open(destination_folder + '\\tokens_contextualized_existing.csv','w',newline='') as f:
    write = csv.writer(f)
    write.writerows(nTokens)

## -------------------- get examples
n_examples = 10
expTokens = []
expTokens.append(['compound','tokens_BERT','tokens_Llama'])

random.seed(1)
r_idx = random.sample(range(0,len(compound_names)), n_examples)

for i in range(0,n_examples):
    n_tok_bert = ', '.join(tokenizer_bert.tokenize(compound_names[r_idx[i]])) # BERT
    n_tok_llama = ', '.join(tokenizer_llama.tokenize(compound_names[r_idx[i]]))[1::] # Llama-2-13b (exclude first special character)
    expTokens.append([compound_names[r_idx[i]],n_tok_bert,n_tok_llama])

with open(destination_folder + '\\examples_contextualized_existing.csv','w',newline='',encoding="utf-8") as f:
    write = csv.writer(f)
    write.writerows(expTokens)



## (3) -------------------- NOVEL CONTEXTUALIZED

# import sentences from the self paced reading database (novel compounds)
sentences = []
with open('...\CWE\sentence_novel_spr.csv', newline='') as f:
    reader = csv.reader(f)
    sentences = list(reader)
compound_names = [row[1] for row in sentences][0:len(sentences):74]

# get number of tokens per compound
nTokens = []
nTokens.append(['compound','N_tokens_BERT','N_tokens_Llama'])

for i in range(0,len(compound_names)):
    n_tok_bert = len(tokenizer_bert.tokenize(compound_names[i])) # BERT
    n_tok_llama = len(tokenizer_llama.tokenize(compound_names[i])) # Llama-2-13b
    nTokens.append([compound_names[i],n_tok_bert,n_tok_llama])

# export
with open(destination_folder + '\\tokens_contextualized_novel.csv','w',newline='') as f:
    write = csv.writer(f)
    write.writerows(nTokens)

## -------------------- get examples
n_examples = 10
expTokens = []
expTokens.append(['compound','tokens_BERT','tokens_Llama'])

random.seed(1)
r_idx = random.sample(range(0,len(compound_names)), n_examples)

for i in range(0,n_examples):
    n_tok_bert = ', '.join(tokenizer_bert.tokenize(compound_names[r_idx[i]])) # BERT
    n_tok_llama = ', '.join(tokenizer_llama.tokenize(compound_names[r_idx[i]]))[1::] # Llama-2-13b (exclude first special character)
    expTokens.append([compound_names[r_idx[i]],n_tok_bert,n_tok_llama])

with open(destination_folder + '\\examples_contextualized_novel.csv','w',newline='',encoding="utf-8") as f:
    write = csv.writer(f)
    write.writerows(expTokens)