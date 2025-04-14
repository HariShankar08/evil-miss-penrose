
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
MODEL = ''

login(TOKEN)

parser = argparse.ArgumentParser()
parser.add_argument("--layer", help="LLaMA layer",
                    type=int)
args = parser.parse_args()

LAYER = args.layer

# import sentences from the self paced reading database (existing compounds)
sentences = []
with open('./sentence_existing_spr.csv', newline='') as f:
    reader = csv.reader(f)
    sentences = list(reader)

########## COMPUTING
# load model
#model = cwe.CWE('../llama_13B_hf_weights/', device= 'cuda:0' if torch.cuda.is_available() else 'cpu')
#model = cwe.CWE('../llama_13B_hf_weights/', device= 'cpu', llama=True)
device = 'cuda:0' if torch.cuda.is_available() else 'cpu' # "cpu"
# model_name = "meta-llama/Llama-2-13b-hf" #'../llama_13B_hf_weights/'
model = cwe.CWE(MODEL, device= device, llama=True)

# compute contextualized word embeddings in batches
res = []
stimuli_dl = DataLoader(sentences, batch_size = 16)

for batch in tqdm(stimuli_dl):

    w1, w2 = batch
    s = list(zip(w1,w2))
    vecs = model.extract_representation(s, layer = LAYER)
    res.append(vecs)

V = concat_list = [j for i in res for j in i]
V = torch.stack(V).to(device=device)

# compute cosine similarities
cos = torch.nn.CosineSimilarity(dim=0).to(device=device)
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

destination_folder = '../CWE_output_llama/spr_novel/'
os.makedirs(destination_folder, exist_ok=True)

# save cosine values
with open(destination_folder+"cosines_existing_spr_llama"+"_layer_"+str(LAYER)+".csv",'w') as f:
    write = csv.writer(f)
    write.writerows(all_sims)

# save contextualized embeddings
npV = V.cpu().numpy() 
df = pd.DataFrame(npV) # convert to a dataframe
df2 = df.assign(sentence = sentences) # add a column with the sentence
df2.to_csv(destination_folder+"embeddings_existing_spr_llama"+"_layer_"+str(LAYER)+".csv", index = False)


