library(tidyverse)
library(ranger)
library(ggplot2)

# ---------------------------------------------------------
# LOAD DATA
# ---------------------------------------------------------
df <- read.csv("spotify_dataset_clean.csv", stringsAsFactors = FALSE)
colnames(df) <- make.names(colnames(df))

contexts <- c(
  "Good.for.Party","Good.for.Work.Study","Good.for.Relaxation.Meditation",
  "Good.for.Exercise","Good.for.Running","Good.for.Driving",
  "Good.for.Yoga.Stretching","Good.for.Social.Gatherings",
  "Good.for.Morning.Routine"
)

sim_cols <- c("Similar.Song.1","Similar.Song.2","Similar.Song.3")

task1_features <- c(
  "Energy","Danceability","Loudness..db.","Positiveness","Tempo",
  "Acousticness","Speechiness","Instrumentalness","Liveness",
  "Popularity","Explicit","emotion"
)

# =========================================================
# step 1 — ranger 모델 학습
# =========================================================
models <- list()

for (ctx in contexts) {
  df_train <- df %>% select(all_of(task1_features), Class = all_of(ctx))
  df_train$Class <- factor(ifelse(df_train$Class == 1, "Yes", "No"))
  
  models[[ctx]] <- ranger(
    Class ~ .,
    data = df_train,
    num.trees = 100,
    probability = TRUE,
    num.threads = parallel::detectCores()
  )
}

# =========================================================
# step 2 — Similarity Index
# =========================================================
sim_mat <- df[, sim_cols]
song_idx <- match(as.matrix(sim_mat), df$song)
song_idx <- matrix(song_idx, ncol = 3)

# =========================================================
# BASELINE
# =========================================================
baseline_match <- data.frame(Context = contexts, Baseline = 0)

for (i in seq_along(contexts)) {
  ctx <- contexts[i]
  orig_pos <- which(df[[ctx]] == 1)
  
  sim_lab <- matrix(df[[ctx]][song_idx], ncol = 3)
  baseline_match$Baseline[i] <- mean(sim_lab[orig_pos, ] == 1, na.rm = TRUE)
}

# =========================================================
# step1 확률 prob_mat
# =========================================================
prob_mat <- matrix(0, nrow(df), length(contexts))
colnames(prob_mat) <- contexts

for (i in seq_along(contexts)) {
  ctx <- contexts[i]
  pred <- predict(models[[ctx]], df[, task1_features])
  prob_mat[, i] <- pred$predictions[, "Yes"]
}

# =========================================================
# similarity 후보 확률 sim_prob
# =========================================================
sim_prob <- array(NA, dim = c(nrow(df), 3, length(contexts)),
                  dimnames = list(NULL, paste0("Sim",1:3), contexts))

for (k in seq_along(contexts)) {
  for (j in 1:3) {
    idx <- song_idx[, j]
    valid <- !is.na(idx)
    
    tmp <- rep(NA, nrow(df))
    tmp[valid] <- prob_mat[idx[valid], k]
    
    sim_prob[, j, k] <- tmp
  }
}

# =========================================================
# Improved — TOP-2 FILTERING (추천시스템 실전 방식)
# =========================================================
improved_match <- data.frame(Context = contexts, Improved = 0)

for (i in seq_along(contexts)) {
  ctx <- contexts[i]
  orig_pos <- which(df[[ctx]] == 1)
  
  score_mat <- sim_prob[orig_pos, , i]
  
  if (all(is.na(score_mat))) {
    improved_match$Improved[i] <- NA
    next
  }
  
  # 각 노래의 similar 3곡 중 확률 상위 2곡 유지
  kept_ratio <- apply(score_mat, 1, function(x) {
    if (all(is.na(x))) return(NA)
    sx <- sort(x, decreasing = TRUE)
    thr <- sx[min(2, length(sx))]          # 두 번째로 높은 확률
    mean(x >= thr, na.rm = TRUE)
  })
  
  improved_match$Improved[i] <- mean(kept_ratio, na.rm = TRUE)
}

# =========================================================
# SUMMARY
# =========================================================
step2_summary <- baseline_match %>%
  left_join(improved_match, by = "Context") %>%
  mutate(Gain = Improved - Baseline)

print(step2_summary)
write.csv(step2_summary, "step2_summary(top2).csv", row.names = FALSE)

# =========================================================
# PLOT
# =========================================================
ggplot(step2_summary, aes(x = reorder(Context, Gain), y = Gain)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(title = "step2 – Situation-Aware Gain (Top 2)",
       x = "", y = "Gain") +
  theme_minimal(base_size = 15)

ggsave("step2_gain_plot(top2).png", width = 8, height = 6)

