###############################################################
# BASELINE FULL VECTORIZED PIPELINE
# Step1(RandomForest) + Step2(Vectorized) + Step3(Vectorized)
###############################################################

suppressMessages({
  library(tidyverse)
  library(randomForest)
  library(lsa)
})

log_msg <- function(x) cat("[LOG]", x, "\n")

###############################################################
# (0) LOAD DATA
###############################################################

log_msg("Loading dataset...")

df <- read.csv("spotify_dataset_clean.csv", stringsAsFactors = FALSE)
colnames(df) <- make.names(colnames(df))

# Loudness numeric 변환 (원본 규칙 유지)
df$Loudness..db. <- as.numeric(gsub("db", "", df$Loudness..db.))

# row_id 추가 (벡터화 Step2-B 필수)
df$row_id <- seq_len(nrow(df))

###############################################################
# (1) STEP1 — RandomForest Models 
###############################################################

input_cols <- c(
  "Energy","Danceability","Loudness..db.","Positiveness","Tempo",
  "Acousticness","Speechiness","Instrumentalness","Liveness",
  "Popularity","lyrics_len","year","title_len","emotion","Explicit"
)

target_cols <- c(
  "Good.for.Party","Good.for.Work.Study","Good.for.Relaxation.Meditation",
  "Good.for.Exercise","Good.for.Running","Good.for.Driving",
  "Good.for.Yoga.Stretching","Good.for.Social.Gatherings",
  "Good.for.Morning.Routine"
)

df <- na.omit(df)

train_models <- function(df, input_cols, target_cols) {
  
  models <- list()
  
  for (tg in target_cols) {
    log_msg(paste("Training RF model for:", tg))
    
    temp <- df %>%
      select(all_of(input_cols), all_of(tg)) %>%
      mutate(Label = factor(ifelse(.data[[tg]] == 1, "Yes", "No"))) %>%
      select(-all_of(tg))
    
    # Down-sampling (원본 유지)
    y <- temp$Label
    x <- temp[, input_cols]
    
    down_train <- caret::downSample(x, y)
    
    rf <- randomForest(
      Class ~ ., data = down_train,
      ntree = 150, nodesize = 5, importance = TRUE
    )
    
    models[[tg]] <- rf
  }
  return(models)
}

log_msg("=== STEP1: Training all models ===")
models <- train_models(df, input_cols, target_cols)
saveRDS(models, "context_models_rf_vectorized.rds")


###############################################################
# STEP1 VECTORIZED PREDICTION
###############################################################

predict_context_vectorized <- function(df, models, input_cols) {
  
  X <- df[, input_cols]
  
  mat <- sapply(names(models), function(ctx) {
    predict(models[[ctx]], X, type = "prob")[, "Yes"]
  })
  
  colnames(mat) <- names(models)
  return(mat)    # n x 9
}

###############################################################
# (2) STEP2 — Centroids 
###############################################################

input_cols_step2 <- c(
  "Energy","Danceability","Loudness..db.","Positiveness","Tempo",
  "Acousticness","Speechiness","Instrumentalness","Liveness",
  "Popularity","lyrics_len","year","title_len"
)

compute_centroid <- function(df, ctx, features) {
  df %>%
    filter(.data[[ctx]] == 1) %>%
    select(all_of(features)) %>%
    summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
    as.numeric()
}

log_msg("=== STEP2: Computing centroids ===")
centroids <- lapply(target_cols, function(ctx) compute_centroid(df, ctx, input_cols_step2))
names(centroids) <- target_cols
saveRDS(centroids, "context_centroids_vectorized.rds")


###############################################################
# STEP2-A — Cosine Similarity (Fully Vectorized)
###############################################################

compute_step2A_vectorized <- function(df, centroids, features) {
  
  X <- as.matrix(df[, features])        # n x d
  Xn <- sqrt(rowSums(X * X)) + 1e-12
  
  # centroids list → numeric matrix
  C <- do.call(rbind, centroids)        # 9 x d
  Cn <- sqrt(rowSums(C * C)) + 1e-12
  
  sim <- (X %*% t(C)) / (Xn %o% Cn)
  colnames(sim) <- names(centroids)
  
  return(sim)
}



###############################################################
# STEP2-B — Similar-song membership (Vectorized)
# (song + artist 완전 일치 기준)
###############################################################

log_msg("Transforming similar-song data...")

# LONG format 만들기
sim_long <- df %>%
  select(row_id,
         Similar.Song.1, Similar.Artist.1,
         Similar.Song.2, Similar.Artist.2,
         Similar.Song.3, Similar.Artist.3) %>%
  pivot_longer(
    cols = -row_id,
    names_to = c(".value", "idx"),
    names_pattern = "(Similar\\.Song|Similar\\.Artist)\\.(\\d+)"
  ) %>%
  rename(sim_song = Similar.Song,
         sim_artist = Similar.Artist) %>%
  filter(sim_song != "", sim_artist != "")

# 원본 row_id 보존
sim_long <- sim_long %>% rename(orig_row_id = row_id)

song_info <- df %>%
  select(song, Artist.s., row_id, all_of(target_cols))

log_msg("Vectorized matching (song + artist)...")

matched <- sim_long %>%
  inner_join(
    song_info,
    by = c("sim_song" = "song",
           "sim_artist" = "Artist.s."),
    relationship = "many-to-many"
  )

log_msg("Computing Step2-B membership ratios...")

ratio_df <- matched %>%
  group_by(orig_row_id) %>%     
  summarise(across(all_of(target_cols), ~ mean(.x, na.rm = TRUE))) %>%
  ungroup()

# Step2B matrix 생성
Step2B_mat <- matrix(0, nrow(df), length(target_cols))
colnames(Step2B_mat) <- target_cols

Step2B_mat[ratio_df$orig_row_id, ] <- as.matrix(ratio_df[, target_cols])


###############################################################
# STEP2 FINAL SCORE = 0.6 A + 0.4 B
###############################################################

compute_step2_final <- function(simA, simB, alpha = 0.6) {
  sim_final <- alpha * simA + (1 - alpha) * simB
  return(sim_final)
}

###############################################################
# (3) STEP3 — FINAL SCORE (Vectorized)
###############################################################

step3_vectorized <- function(df, models, centroids,
                             input_cols, input_cols_step2,
                             w1 = 0.5, w2 = 0.5) {
  
  log_msg("=== STEP3 START ===")
  
  log_msg("Predicting Step1 probabilities...")
  P1 <- predict_context_vectorized(df, models, input_cols)
  
  log_msg("Computing Step2-A similarities...")
  S2A <- compute_step2A_vectorized(df, centroids, input_cols_step2)
  
  log_msg("Loading Step2-B matrix...")
  S2B <- Step2B_mat
  
  log_msg("Combining Step2 scores...")
  S2 <- compute_step2_final(S2A, S2B, alpha = 0.6)
  
  log_msg("Computing final Step3 scores...")
  F <- w1 * P1 + w2 * S2   # n x 9
  
  result_df <- cbind(
    df[, c("row_id", "song", "Artist.s.")],
    as.data.frame(F)
  )
  
  colnames(result_df)[3] <- "artist"
  
  log_msg("STEP3 DONE.")
  
  # ★ return 값을 리스트로 묶어서 내보냄
  return(list(
    scores = result_df,  # Step3 최종 결과
    P1 = P1,             # Step1 확률 행렬
    S2 = S2              # Step2 최종 similarity 행렬
  ))
}


###############################################################
# EXECUTE BASELINE 
###############################################################

log_msg("Running full vectorized baseline...")

out <- step3_vectorized(
  df = df,
  models = models,
  centroids = centroids,
  input_cols = input_cols,
  input_cols_step2 = input_cols_step2
)

baseline_scores <- out$scores   # Step3 최종 스코어 테이블
P1 <- out$P1                    # Step1 확률 행렬
S2 <- out$S2                    # Step2 최종 similarity 행렬

write.csv(baseline_scores, "baseline_scores_vectorized.csv", row.names = FALSE)
saveRDS(baseline_scores, "baseline_scores_vectorized.rds")

log_msg("All baseline outputs saved.")


###############################################################
# STEP4 — PROJECT VISUALIZATION SET
###############################################################

library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)

scores <- baseline_scores
contexts <- target_cols

log_msg("=== Generating PROJECT visualization set ===")

###############################################################
# (A) cor_mat 먼저 생성
###############################################################
cor_mat <- cor(scores[, contexts])

###############################################################
# (B) top30_all 먼저 생성
###############################################################
top30_all <- data.frame()

for (ctx in contexts) {
  t <- scores %>%
    arrange(desc(.data[[ctx]])) %>%
    slice(1:30) %>%
    select(song, artist, !!ctx) %>%
    mutate(context = ctx)
  
  colnames(t)[3] <- "score"
  top30_all <- rbind(top30_all, t)
}

###############################################################
# 1) Top30 playlist CSV 저장
###############################################################

log_msg("Saving Top30 playlists...")

for (ctx in contexts) {
  write.csv(
    scores %>% arrange(desc(.data[[ctx]])) %>% slice(1:30),
    paste0("playlist_top30_", ctx, ".csv"),
    row.names = FALSE
  )
}

###############################################################
# 2) Correlation Heatmap
###############################################################

log_msg("Plot: Context Correlation Heatmap")

pdf("plot_context_correlation_heatmap.pdf", width = 9, height = 9)

ggplot(melt(cor_mat), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text()
  ) +
  labs(title = "Correlation Between Context Scores", x = "", y = "")

dev.off()

###############################################################
# 3) Top30 Heatmap
###############################################################

pdf("plot_top30_heatmap.pdf", width = 10, height = 14)

ggplot(top30_all, aes(x = context, y = song, fill = score)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.y = element_text(size = 6),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(title = "Top 30 Songs Heatmap (per Context)",
       x = "Context", y = "Song")

dev.off()

###############################################################
# 4) Step1 vs Step2 Scatter
###############################################################

ctx <- "Good.for.Running"

pdf("plot_step1_vs_step2_scatter.pdf", width = 8, height = 6)

plot(P1[, ctx], S2[, ctx],
     xlab = "Step1 Probability",
     ylab = "Step2 Similarity",
     main = paste("Step1 vs Step2 —", ctx),
     pch = 19, col = rgb(0.1, 0.3, 1, 0.4))

dev.off()

log_msg("=== PROJECT visualization set DONE ===")
