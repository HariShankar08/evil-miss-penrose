
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

# import sentences
sentences = []
with open('./sentence_variants.csv', newline='', encoding = 'utf-8') as f: #
    reader = csv.reader(f)
    sentences = list(reader)
print("sentences loaded")


########## COMPUTING
# load model
#model = cwe.CWE('../llama_13B_hf_weights/', device= 'cuda:0' if torch.cuda.is_available() else 'cpu')
#model = cwe.CWE('../llama_13B_hf_weights/', device= 'cpu', llama=True)
device = 'cuda:0' if torch.cuda.is_available() else 'cpu' # "cpu"
# model_name = "meta-llama/Llama-2-13b-hf" #'../llama_13B_hf_weights/'
model = cwe.CWE(MODEL, device= device, llama=True)#, load_in_4bit=True)

# compute contextualized word embeddings in batches
res = []
stimuli_dl = DataLoader(sentences, batch_size = 12)

for batch in tqdm(stimuli_dl):
    
    w1, w2 = batch
    s = list(zip(w1,w2))
    vecs = model.extract_representation(s, layer = LAYER)
    res.append(vecs)

V = concat_list = [j for i in res for j in i]
V = torch.stack(V).to(device=device)

# compute average representations for each compound and interpretation
all_avg = []
all_avg10 = []
all_avg5 = []
all_avg2 = []

number_of_compounds = 1120
max_sents = 15

for i in tqdm(range(0,number_of_compounds)): # iterating over compounds
    
    for j in range(0,37): # iterating over relational interpretations

        avg = [sentences[i*37*max_sents+j][1]] 
        temp = V[i*37*max_sents+j]
        for k in range(1,max_sents): # iterating over 15 sentences
            temp = torch.add(temp,V[i*37*max_sents+k*37+j])
        avg.append(torch.div(temp,max_sents))
        all_avg.append(avg)

        avg10 = [sentences[i*37*max_sents+j][1]] 
        temp10 = V[i*37*max_sents+j]
        for k in range(1,10): # iterating over 10 sentences
            temp10 = torch.add(temp10,V[i*37*max_sents+k*37+j])
        avg10.append(torch.div(temp10,10))
        all_avg10.append(avg10)

        avg5 = [sentences[i*37*max_sents+j][1]] 
        temp5 = V[i*37*max_sents+j]
        for k in range(1,5): # iterating over 5 sentences
            temp5 = torch.add(temp5,V[i*37*max_sents+k*37+j])
        avg5.append(torch.div(temp5,5))
        all_avg5.append(avg5)

        avg2 = [sentences[i*37*max_sents+j][1]] 
        temp2 = V[i*37*max_sents+j]
        for k in range(1,2): # iterating over 2 sentences
            temp2 = torch.add(temp2,V[i*37*max_sents+k*37+j])
        avg2.append(torch.div(temp2,2))
        all_avg2.append(avg2)

        
# compute cosine similarities
cos = torch.nn.CosineSimilarity(dim=0).to(device=device)
sims = []
all_sims = []
sims10 = []
all_sims10 = []
sims5 = []
all_sims5 = []
sims2 = []
all_sims2 = []
interpretations = ['compound','H_about_M','H_by_M','H_caused_by_M','H_causes_M',
                   'H_causing_M','H_that_causes_M','H_during_M','H_for_M','H_from_M',
                   'H_has_M','H_having_M','H_that_has_M','M_has_H','M_having_H',
                   'M_that_has_H','H_is_M','H_being_M','H_that_is_M','H_location_is_M',
                   'H_whose_location_is_M','H_located_on_M','M_location_is_H',
                   'M_whose_location_is_H','M_located_on_H','H_made_of_M','H_makes_M',
                   'H_making_M','H_that_makes_M','H_used_by_M','H_uses_M','H_using_M',
                   'H_that_uses_M','H_located_in_M','H_located_at_M','M_located_in_H',
                   'M_located_at_H']
all_sims.append(interpretations)
all_sims10.append(interpretations)
all_sims5.append(interpretations)
all_sims2.append(interpretations)

for i in tqdm(range(0,number_of_compounds)): # iterating over compounds
    sims = [sentences[i*37*max_sents][1]]
    sims10 = [sentences[i*37*max_sents][1]]
    sims5 = [sentences[i*37*max_sents][1]]
    sims2 = [sentences[i*37*max_sents][1]]
    for j in range(1,37): # iterating over relational interpretations
        sims.append(float(cos(all_avg[i*37][1],all_avg[i*37+j][1])))
        sims10.append(float(cos(all_avg10[i*37][1],all_avg10[i*37+j][1])))
        sims5.append(float(cos(all_avg5[i*37][1],all_avg5[i*37+j][1])))
        sims2.append(float(cos(all_avg2[i*37][1],all_avg2[i*37+j][1])))
    all_sims.append(sims)
    all_sims10.append(sims10)
    all_sims5.append(sims5)
    all_sims2.append(sims2)



########## SAVING
destination_folder = '../CWE_output_llama/UkWac/'
os.makedirs(destination_folder, exist_ok=True)

# save cosine values from embeddings averaged over sentences
with open(destination_folder+'cosines_variants_llama'+"_layer_"+str(LAYER)+'.csv','w') as f:
    write = csv.writer(f)
    write.writerows(all_sims)
with open(destination_folder+'cosines_variants_10_llama'+"_layer_"+str(LAYER)+'.csv','w') as f:
    write = csv.writer(f)
    write.writerows(all_sims10)
with open(destination_folder+'cosines_variants_5_llama'+"_layer_"+str(LAYER)+'.csv','w') as f:
    write = csv.writer(f)
    write.writerows(all_sims5)
with open(destination_folder+'cosines_variants_2_llama'+"_layer_"+str(LAYER)+'.csv','w') as f:
    write = csv.writer(f)
    write.writerows(all_sims2)


# save contextualized embeddings averaged over sentences
indeces = list(range(0,len(all_avg))) # 15
avg_vecs = [all_avg[i][1] for i in indeces]
avg_vecs = torch.stack(avg_vecs)
npavg_vecs = avg_vecs.numpy()
df = pd.DataFrame(npavg_vecs) # convert to dataframe
names = [all_avg[i][0] for i in indeces]
df2 = df.assign(relational_interpretation = names) # add a column with the relational interpretations
df2.to_csv(destination_folder+"averaged_embeddings_variants_llama"+"_layer_"+str(LAYER)+".csv", index = False)

indeces = list(range(0,len(all_avg10))) # 10
avg_vecs10 = [all_avg10[i][1] for i in indeces]
avg_vecs10 = torch.stack(avg_vecs10)
npavg_vecs10 = avg_vecs10.numpy()
df = pd.DataFrame(npavg_vecs10) # convert to dataframe
names = [all_avg10[i][0] for i in indeces]
df2 = df.assign(relational_interpretation = names) # add a column with the relational interpretations
df2.to_csv(destination_folder+"averaged_embeddings_variants_10_llama"+"_layer_"+str(LAYER)+".csv", index = False)

indeces = list(range(0,len(all_avg5))) # 5
avg_vecs5 = [all_avg5[i][1] for i in indeces]
avg_vecs5 = torch.stack(avg_vecs5)
npavg_vecs5 = avg_vecs5.numpy()
df = pd.DataFrame(npavg_vecs5) # convert to dataframe
names = [all_avg5[i][0] for i in indeces]
df2 = df.assign(relational_interpretation = names) # add a column with the relational interpretations
df2.to_csv(destination_folder+"averaged_embeddings_variants_5_llama"+"_layer_"+str(LAYER)+".csv", index = False)

indeces = list(range(0,len(all_avg2))) # 2
avg_vecs2 = [all_avg2[i][1] for i in indeces]
avg_vecs2 = torch.stack(avg_vecs2)
npavg_vecs2 = avg_vecs2.numpy()
df = pd.DataFrame(npavg_vecs2) # convert to dataframe
names = [all_avg2[i][0] for i in indeces]
df2 = df.assign(relational_interpretation = names) # add a column with the relational interpretations
df2.to_csv(destination_folder+"averaged_embeddings_variants_2_llama"+"_layer_"+str(LAYER)+".csv", index = False)


