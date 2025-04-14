
MAIN FOLDERS (additional information in the subfolders)

models = contains code and data used to generate word embeddings with LLMs and baseline models and the subsequent relation probabilities
relation_probabilities = contains cosine similarities and probability-transformed similarities (relation probabilities) 
analyses = contains R code for statistical analyses and the results of tests of (negative Binomial) model assumptions

RESOURCES REFERENCED IN THE CODE

---- models

## BERT-base-uncased
https://huggingface.co/bert-base-uncased
## Llama-2-13b
https://huggingface.co/meta-llama/Llama-2-13b-hf 
4-bit quantization: https://huggingface.co/docs/transformers/main_classes/quantization
## DISSECT toolkit 
https://wiki.cimec.unitn.it/tiki-index.php?page=CLIC
## word2vec semantic space (Baroni et al., 2014)
https://sites.google.com/site/fritzgntr/software-resources/semantic_spaces?authuser=0

---- behavioral data (possible relation task judgements)

## citation: Benjamin, S., & Schmidtke, D. (2023). Conceptual combination during novel and existing compound word reading in context: A self-paced reading study. Memory & Cognition, 51(5), 1170-1197.
data available at: https://osf.io/5r93v/
## citation: Schmidtke, D., Gagné, C. L., Kuperman, V., & Spalding, T. L. (2018). Language experience shapes relational knowledge of compound words. Psychonomic bulletin & review, 25, 1468-1487.
data available at: https://doi.org/10.3758/s13423-018-1478-x
## Schmidtke, D., Gagné, C. L., Kuperman, V., Spalding, T. L., & Tucker, B. V. (2018). Conceptual relations compete during auditory and visual compound word recognition. Language, cognition and neuroscience, 33(7), 923-942.
data available at: https://osf.io/ycd64/

