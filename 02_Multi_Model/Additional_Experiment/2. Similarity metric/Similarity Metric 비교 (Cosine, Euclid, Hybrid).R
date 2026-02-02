###############################################################
# SCRIPT 2 — Similarity Metric Comparison
# FINAL ULTRA-STABLE VERSION (NO MEMORY ERRORS / NO DROP)
###############################################################

library(tidyverse)
library(reshape2)
library(ggplot2)

###############################################################
# 1) Load Data & Centroids
###############################################################

df <- read.csv("spotify_dataset_clean.csv", stringsAsFactors = FALSE)
centroids <- readRDS("context_centroids_vectorized.rds")

input_cols_step2 <- c(
  "Energy","Danceability","Loudness..db.","Positiveness","Tempo",
  "Acousticness","Speechiness","Instrumentalness","Liveness",
  "Popularity","lyrics_len","year","title_len"
)

###############################################################
# 2) Force numeric on all Step2 features
###############################################################

df[input_cols_step2] <- lapply(df[input_cols_step2], function(x){
  suppressWarnings(as.numeric(x))
})

# Replace remaining NA with 0 (cosine/euclid-safe)
df[input_cols_step2][is.na(df[input_cols_step2])] <- 0

###############################################################
# 3) Convert to matrices
###############################################################

X <- as.matrix(df[, input_cols_step2])

C <- do.call(rbind, centroids)
C <- as.matrix(C)

###############################################################
# 4) SAFE cosine / euclid / hybrid functions
###############################################################

# ---- Cosine similarity ----
cosine_sim <- function(X, C){
  Xn <- sqrt(rowSums(X * X)) + 1e-12
  Cn <- sqrt(rowSums(C * C)) + 1e-12
  sim <- (X %*% t(C)) / (Xn %o% Cn)
  as.matrix(sim)   # prevent dimension drop
}

# ---- Euclidean similarity (vectorized, NO dist()) ----
euclid_sim <- function(X, C){
  X2 <- rowSums(X * X)
  C2 <- rowSums(C * C)
  
  # dist^2 = (X*X)_row + (C*C)_row - 2 * (X %*% t(C))
  dist2 <- outer(X2, C2, "+") - 2 * (X %*% t(C))
  dist2 <- pmax(dist2, 0)     # avoid negative due to floating errors
  
  dist <- sqrt(dist2)
  as.matrix(-dist)            # convert distance → similarity
}

# ---- Hybrid similarity ----
hybrid_sim <- function(cosine, euclid, w = 0.7){
  as.matrix( w * cosine + (1 - w) * euclid )
}

###############################################################
# 5) Compute three similarity matrices
###############################################################

S_cosine <- cosine_sim(X, C)
S_euclid <- euclid_sim(X, C)
S_hybrid <- hybrid_sim(S_cosine, S_euclid)

contexts <- names(centroids)

colnames(S_cosine) <- contexts
colnames(S_euclid) <- contexts
colnames(S_hybrid) <- contexts

write.csv(S_cosine, "similarity_cosine.csv", row.names = FALSE)
write.csv(S_euclid, "similarity_euclid.csv", row.names = FALSE)
write.csv(S_hybrid, "similarity_hybrid.csv", row.names = FALSE)

###############################################################
# 6) Ranking consistency comparison (Top-50 overlap)
###############################################################

compare_ranking <- function(sim1, sim2, ctx){
  rank1 <- order(sim1[, ctx], decreasing = TRUE)
  rank2 <- order(sim2[, ctx], decreasing = TRUE)
  
  length(intersect(rank1[1:50], rank2[1:50])) / 50
}

ranking_results <- data.frame()

for (ctx in contexts){
  ranking_results <- rbind(ranking_results, data.frame(
    context = ctx,
    cosine_vs_euclid = compare_ranking(S_cosine, S_euclid, ctx),
    cosine_vs_hybrid = compare_ranking(S_cosine, S_hybrid, ctx),
    euclid_vs_hybrid = compare_ranking(S_euclid, S_hybrid, ctx)
  ))
}

write.csv(ranking_results, "similarity_metric_ranking_overlap.csv", row.names = FALSE)

###############################################################
# 7) Visualization: Heatmap
###############################################################

df_plot <- melt(ranking_results, id.vars = "context")

pdf("plot_similarity_metric_overlap.pdf", width = 8, height = 6)
ggplot(df_plot, aes(x = variable, y = context, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Ranking Overlap Across Similarity Metrics",
    x = "",
    y = ""
  )
dev.off()

cat("SCRIPT 2 DONE — FINAL ULTRA-STABLE VERSION.\n")

