# Network of symptoms in Major depression disorder 
# BDI items

# Load packages ----
library(bootnet)
library(dplyr)
library(purrr)

# Splitting datasets ----
symptoms_df <- readRDS("data/merge_symptoms_sa_dataset.rds")
dim(symptoms_df)
head(symptoms_df)

summary(symptoms_df)

selected_vars <-  c("HAM1", "HAM2", "HAM7", "HAM8", "HAM10", "HAM13")
selected_vars

symptoms_df <- symptoms_df[, selected_vars]
dim(symptoms_df)
head(symptoms_df)

library(dplyr)
# depressed mood, guilt, work and interests, psychomotor retardation, psychic anxiety and general somatic symptoms

nodes_labs <- c("Mood", "Guilt", "Wk Ac", "Ret", "An Pch", "G Som")

long_labs <- c("Depressed Mood", "Feelings Of Guilt", "Work And Activities", "Retardation", "Anxiety Psychic",
               "General Somatic Symptoms")

labs_df <- data.frame(item = colnames(symptoms_df), nodes_labs, long_labs)
labs_df

df <- scale(symptoms_df)
colnames(df) <- nodes_labs

# Estimating networks ----
model_net <- estimateNetwork(df, default = c("EBICglasso"))

plot(model_net, labels = nodes_labs)
model_net$labels <- nodes_labs

library(qgraph)

# Centrality ----
centralityPlot(model_net, include = c("Strength", "Betweenness", "Closeness"))
sort(centrality(model_net)$OutDegree)

# Bootstrap ----
set.seed(1234)
boot_case <- bootnet(model_net, nBoots = 2500, type = "case", nCores = 2, statistics = c("Strength", "Betweenness", "Closeness"))
corStability(boot_case)

set.seed(1234)
boot_case1000 <- bootnet(model_net, nBoots = 2500, caseN = 1000, type = "case", nCores = 2, statistics = c("Strength", "Betweenness", "Closeness"))
corStability(boot_case1000)

set.seed(1234)
boot <- bootnet(model_net, nBoots = 2500, nCores = 2)

set.seed(1234)
boot_npar <- bootnet(model_net, nBoots = 2500, type = "nonparametric", nCores = 2, statistics = c("Strength", "Betweenness", "Closeness"))

# Bootstrap charts
plot(boot)
plot(boot_npar, statistics = c("Strength", "Betweenness", "Closeness"))
plot(boot_case, statistics = c("Strength", "Betweenness", "Closeness"))
plot(boot_case_better, statistics = c("Strength", "Betweenness", "Closeness"))


# Combining similar nodes
library(networktools)
net_gb <- goldbricker(df, threshold = 0.5)
net_gb$suggested_reductions
net_gb$threshold

#write.table(labs_df, file = "cache/hdrs_labs_table.txt", row.names = FALSE, sep = "\t")
save.image("session/session_hdrs6_networks.RData")
#load("session/session_hdrs6_networks.RData")
