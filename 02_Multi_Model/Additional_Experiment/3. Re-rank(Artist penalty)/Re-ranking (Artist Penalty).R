###############################################################
# SCRIPT 3 — Re-ranking with Artist / Genre Penalty
# FINAL ULTRA-STABLE VERSION (NO ERRORS)
###############################################################

library(tidyverse)

# Load baseline scores
scores <- read.csv("baseline_scores_vectorized.csv", stringsAsFactors = FALSE)

contexts <- c(
  "Good.for.Party","Good.for.Work.Study","Good.for.Relaxation.Meditation",
  "Good.for.Exercise","Good.for.Running","Good.for.Driving",
  "Good.for.Yoga.Stretching","Good.for.Social.Gatherings",
  "Good.for.Morning.Routine"
)

###############################################################
# 1) Artist Penalty Function (SAFE VERSION)
###############################################################

apply_artist_penalty <- function(df, ctx, lambda = 0.02) {
  
  df <- df %>% arrange(desc(.data[[ctx]]))
  
  # assign order per artist
  df$artist_count <- ave(df$artist, df$artist, FUN = seq_along)
  df$artist_count <- as.numeric(df$artist_count)   # FIX: ensure numeric
  
  # apply penalty
  df$penalized <- df[[ctx]] - lambda * (df$artist_count - 1)
  
  df <- df %>% arrange(desc(penalized))
  return(df)
}

###############################################################
# 2) Genre Penalty Function (SAFE VERSION)
###############################################################

apply_genre_penalty <- function(df, ctx, gamma = 0.02) {
  
  if (!"Genre" %in% colnames(df)) {
    message("No Genre column found. Skipping genre penalty.")
    return(NULL)
  }
  
  df <- df %>% arrange(desc(.data[[ctx]]))
  
  df$genre_count <- ave(df$Genre, df$Genre, FUN = seq_along)
  df$genre_count <- as.numeric(df$genre_count)
  
  df$penalized <- df[[ctx]] - gamma * (df$genre_count - 1)
  
  df <- df %>% arrange(desc(penalized))
  return(df)
}

###############################################################
# 3) Run Re-ranking and Save Output
###############################################################

for (ctx in contexts) {
  
  # Artist penalty version
  df_artist <- apply_artist_penalty(scores, ctx)
  write.csv(df_artist, paste0("rerank_artist_", ctx, ".csv"), row.names = FALSE)
  
  # Genre penalty version (only if Genre exists)
  if ("Genre" %in% colnames(scores)) {
    df_genre <- apply_genre_penalty(scores, ctx)
    write.csv(df_genre, paste0("rerank_genre_", ctx, ".csv"), row.names = FALSE)
  }
}

###############################################################
# 4) Measure Artist Diversity Before/After Re-ranking
###############################################################

calc_top30_artist_div <- function(df, ctx) {
  top30 <- df %>% arrange(desc(.data[[ctx]])) %>% slice(1:30)
  length(unique(top30$artist))
}

div_res <- data.frame()

for (ctx in contexts) {
  
  base_div <- calc_top30_artist_div(scores, ctx)
  
  art_df <- read.csv(paste0("rerank_artist_", ctx, ".csv"))
  art_div <- length(unique(art_df$artist[1:30]))
  
  div_res <- rbind(div_res, data.frame(
    context = ctx,
    baseline = base_div,
    after_artist_penalty = art_div
  ))
}

write.csv(div_res, "rerank_diversity_results.csv", row.names = FALSE)

###############################################################
# 5) Visualization — Heatmap of Diversity Change
###############################################################

library(reshape2)

df_plot <- melt(div_res, id.vars = "context")

pdf("plot_rerank_artist_diversity.pdf", width = 8, height = 6)
ggplot(df_plot, aes(x = variable, y = context, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "green") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Artist Diversity Before/After Re-ranking (Top30)",
    x = "",
    y = ""
  )
dev.off()

cat("SCRIPT 3 DONE — FINAL ULTRA-STABLE VERSION.\n")
