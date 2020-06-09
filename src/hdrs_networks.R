# Network of symptoms in Major depression disorder 
# HDRS items

# Load packages ----
library(bootnet)
library(NetworkComparisonTest)
library(dplyr)

# Splitting datasets ----
symptoms_df <- readRDS("data/merge_symptoms_sa_dataset.rds")
dim(symptoms_df)
head(symptoms_df)

selected_vars <- c(colnames(symptoms_df)[grepl("HAM", colnames(symptoms_df))])
selected_vars

symptoms_df <- symptoms_df[, selected_vars]
dim(symptoms_df)
head(symptoms_df)

nodes_labs <- c("Mood", "Suic", "Guilt", "Ins Ni", "Ins Mi", "Ins Mo", "Wk Ac", "Ret", "Agi", 
                "An Pch", "An Som", "Som GI", "G Som", "Genit", "Hyp", "Ls W", "Cons", "Cons")

long_labs <- c("Depressed Mood", "Suicide", "Feelings Of Guilt", "Insomnia: Early In The Night", "Insomnia: Middle Of The Night",
               "Insomnia: Early Hours Of The Morning", "Work And Activities", "Retardation", "Agitation", "Anxiety Psychic",
               "Anxiety Somatic", "Somatic Symptoms Gastro-intestinal", "General Somatic Symptoms", "Genital Symptoms",
               "Hypochondriasis", "Loss Of Weight", "Consciousness")

labs_df <- data.frame(item = colnames(symptoms_df), nodes_labs, long_labs)
labs_df

df <- scale(symptoms_df)

# Estimating networks ----
model_net <- estimateNetwork(df, default = c("EBICglasso"))

plot(model_net, labels = nodes_labs)

library(qgraph)

# Bootstrap ----
set.seed(1234)
boot_case <- bootnet(model_net, nBoots = 1000, type = "case", nCores = 2, statistics = c("Strength", "Betweenness", "Closeness"))

set.seed(1234)
boot <- bootnet(model_net, nBoots = 1000, nCores = 2)

set.seed(1234)
boot_npar <- bootnet(model_net, nBoots = 1000, type = "nonparametric", nCores = 2, statistics = c("Strength", "Betweenness", "Closeness"))

# Bootstrap charts
plot(boot)

plot(boot_case, statistics = c("Strength", "Betweenness", "Closeness"))

# Combining similar nodes
library(networktools)
net_gb <- goldbricker(df, threshold = 0.5)
net_gb$suggested_reductions
net_gb$threshold

save.image("cache/session_hdrs_networks.RData")
