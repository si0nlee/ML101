###############################################################
# SCRIPT 1 — Top-N Experiment (Top30 / Top100 / Top150)
###############################################################

library(tidyverse)

# 1) Load baseline result
scores <- read.csv("baseline_scores_vectorized.csv", stringsAsFactors = FALSE)

contexts <- c(
  "Good.for.Party","Good.for.Work.Study","Good.for.Relaxation.Meditation",
  "Good.for.Exercise","Good.for.Running","Good.for.Driving",
  "Good.for.Yoga.Stretching","Good.for.Social.Gatherings",
  "Good.for.Morning.Routine"
)

###############################################################
# Create Top-N Lists
###############################################################

make_topN <- function(df, ctx, N){
  df %>% arrange(desc(.data[[ctx]])) %>% slice(1:N)
}

topN_list <- list()

for (ctx in contexts){
  topN_list[[ctx]] <- list(
    top30 = make_topN(scores, ctx, 30),
    top100 = make_topN(scores, ctx, 100),
    top150 = make_topN(scores, ctx, 150)
  )
}

###############################################################
# Overlap 계산 (Jaccard)
###############################################################

jaccard <- function(a, b){
  inter <- length(intersect(a, b))
  union <- length(union(a, b))
  inter / union
}

overlap_results <- data.frame()

for (ctx in contexts){
  s30  <- topN_list[[ctx]]$top30$song
  s100 <- topN_list[[ctx]]$top100$song
  s150 <- topN_list[[ctx]]$top150$song
  
  overlap_results <- rbind(overlap_results, data.frame(
    context = ctx,
    Jaccard_30_100 = jaccard(s30, s100),
    Jaccard_30_150 = jaccard(s30, s150),
    Jaccard_100_150 = jaccard(s100, s150)
  ))
}

write.csv(overlap_results, "topN_overlap_results.csv", row.names = FALSE)


###############################################################
# Artist Diversity / Genre Coverage
###############################################################

# Genre는 없을 수 있으니 optional
if ("Genre" %in% colnames(scores)) {
  diversity_results <- data.frame()
  
  for (ctx in contexts){
    for (N in c(30,100,150)){
      tmp <- topN_list[[ctx]][[paste0("top",N)]]
      diversity_results <- rbind(diversity_results, data.frame(
        context = ctx,
        N = N,
        artist_diversity = length(unique(tmp$artist)),
        genre_diversity  = length(unique(tmp$Genre))
      ))
    }
  }
  
  write.csv(diversity_results, "topN_diversity_results.csv", row.names = FALSE)
}

###############################################################
# Plot: Overlap Heatmap
###############################################################

library(reshape2)
library(ggplot2)

df_plot <- melt(overlap_results, id.vars="context")

pdf("plot_topN_overlap_heatmap.pdf", width=8, height=6)
ggplot(df_plot, aes(x=variable, y=context, fill=value)) +
  geom_tile() +
  scale_fill_gradient(low="white", high="red") +
  theme_minimal() +
  labs(title="Top-N Overlap (Jaccard)", x="", y="")
dev.off()

cat("SCRIPT 1 DONE.\n")
