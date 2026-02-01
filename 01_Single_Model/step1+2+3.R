library(tidyverse)
library(ranger)
library(ggplot2)

# =========================================================
# LOAD DATA
# =========================================================
df <- read.csv("spotify_dataset_clean.csv", stringsAsFactors = FALSE)
colnames(df) <- make.names(colnames(df))

contexts <- c(
  "Good.for.Party","Good.for.Work.Study","Good.for.Relaxation.Meditation",
  "Good.for.Exercise","Good.for.Running","Good.for.Driving",
  "Good.for.Yoga.Stretching","Good.for.Social.Gatherings",
  "Good.for.Morning.Routine"
)

sim_cols <- c("Similar.Song.1","Similar.Song.2","Similar.Song.3")

# =========================================================
# FEATURE SET
# =========================================================
task1_features <- c(
  "Energy","Danceability","Loudness..db.","Positiveness","Tempo",
  "Acousticness","Speechiness","Instrumentalness","Liveness",
  "Popularity","Explicit","emotion"
)


# =========================================================
# step 1 — 재학습 (Collapse 방지 최종 안정형)
# =========================================================

models <- list()

for (ctx in contexts) {
  
  df_train <- df %>% 
    select(all_of(task1_features), Class = all_of(ctx))
  
  df_train$Class <- factor(ifelse(df_train$Class == 1, "Yes", "No"))
  
  models[[ctx]] <- ranger(
    Class ~ .,
    data = df_train,
    num.trees = 700,               # 더 깊고 안정적인 확률
    mtry = 3,                      # 과적합 방지
    min.node.size = 50,            # 확률 smooth 핵심
    probability = TRUE,
    importance = "impurity",
    class.weights = c("No" = 1, "Yes" = 120)   # imbalance 강하게 교정
  )
}

saveRDS(models, "context_models_ranger_FINAL.rds")

cat(">>> step1 재학습 완료 (collapse 방지 적용)\n")

cat(">>> step1 결과 확인 시작\n")

# 상황별 확률 분포 출력
for (ctx in contexts) {
  pred_check <- predict(models[[ctx]], df[, task1_features])$predictions[, "Yes"]
  
  cat("\n======", ctx, "======\n")
  print(summary(pred_check))
  
  png(paste0("step1_prob_distribution_", ctx, ".png"), width=900, height=600)
  hist(pred_check, breaks=50, col="skyblue",
       main=paste("step1 Probability Distribution -", ctx),
       xlab="Probability")
  dev.off()
}

cat("\n>>> step1 결과 확인 완료\n")


# =========================================================
# step 2 — Similarity Index 준비
# =========================================================
sim_mat <- df[, sim_cols]
song_idx <- match(as.matrix(sim_mat), df$song)
song_idx <- matrix(song_idx, ncol = 3)


# =========================================================
# BASELINE 계산
# =========================================================
baseline_match <- data.frame(Context = contexts, Baseline = 0)

for (i in seq_along(contexts)) {
  
  ctx <- contexts[i]
  
  orig_pos <- which(df[[ctx]] == 1)
  sim_lab <- matrix(df[[ctx]][song_idx], ncol = 3)
  
  baseline_match$Baseline[i] <- mean(sim_lab[orig_pos, ] == 1, na.rm = TRUE)
}


# =========================================================
# step1 확률 예측 (prob_mat)
# =========================================================
prob_mat <- matrix(0, nrow(df), length(contexts))
colnames(prob_mat) <- contexts

for (i in seq_along(contexts)) {
  ctx <- contexts[i]
  pred <- predict(models[[ctx]], df[, task1_features])
  prob_mat[, i] <- pred$predictions[, "Yes"]
}

cat(">>> step1 확률 예측 prob_mat 생성 완료\n")


# =========================================================
# similarity 후보 확률 sim_prob 생성
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
# Improved 계산
# =========================================================
threshold <- 0.5
improved_match <- data.frame(Context = contexts, Improved = 0)

for (i in seq_along(contexts)) {
  
  ctx <- contexts[i]
  
  orig_pos <- which(df[[ctx]] == 1)
  if (length(orig_pos) == 0) {
    improved_match$Improved[i] <- NA
    next
  }
  
  score_mat <- sim_prob[orig_pos, , i]
  kept <- score_mat >= threshold
  
  improved_match$Improved[i] <- mean(kept, na.rm = TRUE)
}


# =========================================================
# SUMMARY TABLE
# =========================================================
step2_summary <- baseline_match %>%
  left_join(improved_match, by = "Context") %>%
  mutate(Gain = Improved - Baseline)

print(step2_summary)
write.csv(step2_summary, "step2_summary_FINAL.csv", row.names = FALSE)

cat(">>> step2 Summary 저장 완료\n")


# =========================================================
# step 2 PLOT 저장
# =========================================================
ggplot(step2_summary, aes(x = reorder(Context, Gain), y = Gain)) +
  geom_col(fill = "purple") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(
    title = "step2 – Situation-Aware Gain (FINAL)",
    x = "", y = "Gain"
  ) +
  theme_minimal(base_size = 15)


ggsave("step2_gain_plot_FINAL.png", width = 8, height = 6)

# =========================================================
# step 3 — Final Playlist 생성
# =========================================================
dir.create("playlists_FINAL", showWarnings = FALSE)
dir.create("playlist_plots_FINAL", showWarnings = FALSE)
dir.create("feature_importance_FINAL", showWarnings = FALSE)

N <- 30  # top 30 songs

for (ctx in contexts) {
  
  pred <- predict(models[[ctx]], df[, task1_features])$predictions[, "Yes"]
  
  top_idx <- order(pred, decreasing = TRUE)[1:N]
  
  playlist <- df[top_idx, c("Artist.s.","song","Popularity","Tempo","Energy",
                            "Danceability","Loudness..db.","emotion")]
  
  write.csv(playlist, paste0("playlists_FINAL/playlist_", ctx, ".csv"), row.names = FALSE)
  
  # Playlist distribution plot
  p <- ggplot(playlist, aes(x = reorder(song, Popularity), y = Popularity)) +
    geom_col(fill = "orange") +
    coord_flip() +
    labs(title = paste("Top 30 Playlist -", ctx),
         x = "Song", y = "Popularity") +
    theme_minimal(base_size = 15)
  
  ggsave(paste0("playlist_plots_FINAL/playlist_plot_", ctx, ".png"),
         p, width = 9, height = 10)
  
  # Feature Importance 저장
  imp <- data.frame(
    Feature = task1_features,
    Importance = models[[ctx]]$variable.importance
  ) %>% arrange(desc(Importance))
  
  p2 <- ggplot(imp, aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(title = paste("Feature Importance -", ctx),
         x = "", y = "Importance") +
    theme_minimal(base_size = 15)
  
  ggsave(paste0("feature_importance_FINAL/feature_importance_", ctx, ".png"),
         p2, width = 8, height = 6)
}

cat(">>> step3 완료 — 모든 playlist + 시각화 생성됨\n")
