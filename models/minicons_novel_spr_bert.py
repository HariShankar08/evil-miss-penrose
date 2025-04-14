
########## LOADING

# load packages
import torch
import csv
import numpy as np
import pandas as pd
from minicons import cwe
from torch.utils.data import DataLoader
import argparse
from tqdm import tqdm
import os
from huggingface_hub import login

TOKEN = os.getenv('HF_TOKEN')
MODEL = 'FacebookAI/xlm-roberta-base'

login(TOKEN)

parser = argparse.ArgumentParser()
parser.add_argument("--layer", help="Bert layer",
                    type=int)
args = parser.parse_args()

LAYER = args.layer

# import sentences from the self paced reading database (novel compounds)
sentences = []
with open('./sentence_novel_spr.csv', newline='') as f:
    reader = csv.reader(f)
    sentences = list(reader)



########## COMPUTING

# load bert-base-uncased model (saved locally)
model = cwe.CWE(MODEL, device= 'cuda:0' if torch.cuda.is_available() else 'cpu')

# compute contextualized word embeddings in batches
res = []
stimuli_dl = DataLoader(sentences, batch_size = 185)

for batch in tqdm(stimuli_dl):
    
    w1, w2 = batch
    s = list(zip(w1,w2))
    vecs = model.extract_representation(s, layer = LAYER)
    res.append(vecs)

V = concat_list = [j for i in res for j in i]
V = torch.stack(V)

# compute cosine similarities
cos = torch.nn.CosineSimilarity(dim=0)
sims = []
all_sims = []
interpretations = ['compound','sentence','H_about_M','H_by_M','H_caused_by_M','H_causes_M',
                   'H_causing_M','H_that_causes_M','H_during_M','H_for_M','H_from_M',
                   'H_has_M','H_having_M','H_that_has_M','M_has_H','M_having_H',
                   'M_that_has_H','H_is_M','H_being_M','H_that_is_M','H_location_is_M',
                   'H_whose_location_is_M','H_located_on_M','M_location_is_H',
                   'M_whose_location_is_H','M_located_on_H','H_made_of_M','H_makes_M',
                   'H_making_M','H_that_makes_M','H_used_by_M','H_uses_M','H_using_M',
                   'H_that_uses_M','H_located_in_M','H_located_at_M','M_located_in_H',
                   'M_located_at_H']
all_sims.append(interpretations)

for i in tqdm(range(0,200)):
    sims = [sentences[i*37][1]]
    sims.append(sentences[i*37][0])
    for j in range(1,37):
        sims.append(float(cos(V[i*37],V[i*37+j])))
    all_sims.append(sims)



########## SAVING

destination_folder = '../CWE_output/spr_novel/layer_'+str(LAYER)+'/'
os.makedirs(destination_folder, exist_ok=True)

# save cosine values
with open(destination_folder+'cosines_novel_spr.csv','w') as f:
    write = csv.writer(f)
    write.writerows(all_sims)

# save contextualized embeddings
npV = V.numpy() 
df = pd.DataFrame(npV) # convert to a dataframe
df2 = df.assign(sentence = sentences) # add a column with the sentence
df2.to_csv(destination_folder+"embeddings_novel_spr.csv", index = False)


