# Symptoms clustering
# Clustering method: clique percolation
# Load
#   
#   
# Exports session_cliquepercolation_clustering.RData


library('CliquePercolation')
library(qgraph)

hdrs_env <- new.env()
load("session/session_hdrs_networks.RData", envir = hdrs_env)

bdi_env <- new.env()
load("session/session_bdi_networks.RData", envir = bdi_env)

# HDRS ----

hdrs_w <- qgraph(hdrs_env$model_net$graph) 

### 4. use Clique Percolation by optimizing entropy (based on Shannon information)

# the idea here is to identify the most 'surprising' community partition ... 
# ... defined as low probability of knowing to which community a randomly picked node belongs 
max(hdrs_w$Edgelist$weight)

hdrs_thresholds <- cpThreshold(hdrs_w, method = "weighted", k.range = 3,     
                           I.range = c(seq(0.38, 0.01, by = -0.01)), 
                           threshold = "entropy")

hdrs_thresholds

set.seed(1234)
hdrs_permute <- cpPermuteEntropy(hdrs_w, cpThreshold.object = hdrs_thresholds,
                            n = 100, interval = 0.95)

hdrs_permute$Confidence.Interval     # CI values of entropy
hdrs_permute$Extracted.Rows          # relevant final solutions (everybody else deleted automatically)

hdrs_thresholds$Intensity[which.max(hdrs_thresholds$Entropy.Threshold)]

hdrs_results <- cpAlgorithm(hdrs_w, k = 3, method = "weighted", I=.04) # obtain final Clique Percolation solution to plot

### plot network

#names<-c('B1','B2', 'B3', 'B4', 'B5', 'C1', 'C2', 'D1', 'D2', 'D3', 'D4', 'D5', 'D6', 'D7', 'E1', 'E2', 'E3', 'E4', 'E5', 'E6')
#longnames <- c('Intrusive thoughts', 'Nightmares', 'Flashbacks', 'Emotional cue reactivity', 'Physiological cue reactivity', 'Avoidance of thoughts', 'Avoidance of reminders', 'Trauma-related amnesia', 'Negative beliefs', 'Blame of self or others', 'Negative trauma-related emotions', 'Loss of interest', 'Detachment', 'Restricted affect', 'Irritability/anger', 'Self-destructive/reckless behavior', 'Hypervigilance', 'Exaggerated startle response', 'Difficulty concentrating', 'Sleep disturbance')
#gr1 <- list('Intrusions'=c(1:5), 'Avoidance'=c(6:7), 'Cognition & mood alterations'=c(8:14),'Arousal & reactivity alterations'=c(15:20)) 


#pdf("Network4.pdf", width=8.5, height=5)
hdrs_clique <- cpColoredGraph(hdrs_w, list.of.communities = hdrs_results$list.of.communities.numbers, layout="spring", theme='colorblind',
                    vsize=10, cut=0, border.width=1.5, labels = hdrs_env$labs_df$nodes_labs,
                     border.color='black', legend.cex=.37,
                     edge.width = 2, title ="Clique Percolation (optimizing entropy)")
# nodeNames = longnames, 
#dev.off()

# BDI ----
bdi_w <- qgraph(bdi_env$model_net$graph) 

### 4. use Clique Percolation by optimizing entropy (based on Shannon information)

# the idea here is to identify the most 'surprising' community partition ... 
# ... defined as low probability of knowing to which community a randomly picked node belongs 
round(max(bdi_w$Edgelist$weight), 2)

bdi_thresholds <- cpThreshold(bdi_w, method = "weighted", k.range = 3,     
                               I.range = c(seq(0.2, 0.01, by = -0.01)), 
                               threshold = "entropy")

bdi_thresholds

set.seed(1234)
bdi_permute <- cpPermuteEntropy(bdi_w, cpThreshold.object = bdi_thresholds,
                                 n = 100, interval = 0.95)

bdi_permute$Confidence.Interval     # CI values of entropy
bdi_permute$Extracted.Rows          # relevant final solutions (everybody else deleted automatically)

best_i <- bdi_thresholds$Intensity[which.max(bdi_thresholds$Entropy.Threshold)]
best_i

bdi_thresholds[which.max(bdi_thresholds$Entropy.Threshold), ]

bdi_results <- cpAlgorithm(bdi_w, k = 3, method = "weighted", I = best_i) # obtain final Clique Percolation solution to plot
bdi_results
### plot network

#names<-c('B1','B2', 'B3', 'B4', 'B5', 'C1', 'C2', 'D1', 'D2', 'D3', 'D4', 'D5', 'D6', 'D7', 'E1', 'E2', 'E3', 'E4', 'E5', 'E6')
#longnames <- c('Intrusive thoughts', 'Nightmares', 'Flashbacks', 'Emotional cue reactivity', 'Physiological cue reactivity', 'Avoidance of thoughts', 'Avoidance of reminders', 'Trauma-related amnesia', 'Negative beliefs', 'Blame of self or others', 'Negative trauma-related emotions', 'Loss of interest', 'Detachment', 'Restricted affect', 'Irritability/anger', 'Self-destructive/reckless behavior', 'Hypervigilance', 'Exaggerated startle response', 'Difficulty concentrating', 'Sleep disturbance')
#gr1 <- list('Intrusions'=c(1:5), 'Avoidance'=c(6:7), 'Cognition & mood alterations'=c(8:14),'Arousal & reactivity alterations'=c(15:20)) 


#pdf("Network4.pdf", width=8.5, height=5)
bdi_clique <- cpColoredGraph(bdi_w, list.of.communities = bdi_results$list.of.communities.numbers, layout="spring", theme='colorblind',
                              vsize=8, cut=0, border.width=1.5, labels = bdi_env$labs_df$nodes_labs,
                              border.color='black', legend.cex=.37,
                              edge.width = 2, title ="Clique Percolation (optimizing entropy)")
# nodeNames = longnames, 
#dev.off()

bdi_clique$colors.nodes
bdi_env$labs_df$nodes_labs

save.image("session_cliquepercolation_clustering.RData")
#load("session_cliquepercolation_clustering.RData")
