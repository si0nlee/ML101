library(tidyverse)
library(ranger)
library(pROC)

# =========================
# 데이터 로드
# =========================
df <- read.csv("spotify_dataset_clean.csv", stringsAsFactors = FALSE)
colnames(df) <- make.names(colnames(df))

contexts <- c(
  "Good.for.Party","Good.for.Work.Study","Good.for.Relaxation.Meditation",
  "Good.for.Exercise","Good.for.Running","Good.for.Driving",
  "Good.for.Yoga.Stretching","Good.for.Social.Gatherings",
  "Good.for.Morning.Routine"
)

task1_features <- c(
  "Energy","Danceability","Loudness..db.","Positiveness","Tempo",
  "Acousticness","Speechiness","Instrumentalness","Liveness",
  "Popularity","Explicit","emotion"
)

# =========================
# 모델 로드 (task3 모델)
# =========================
models <- readRDS("context_models_ranger_FINAL.rds")

# =========================
# 성능 저장용 테이블
# =========================
performance_table <- data.frame()

# =========================
# Step1 성능 평가
# =========================
cat("\n========== Step1 성능 평가 시작 ==========\n")

for (ctx in contexts) {
  
  cat("\n\n------", ctx, "------\n")
  
  actual <- df[[ctx]]
  prob <- predict(models[[ctx]], df[, task1_features])$predictions[, "Yes"]
  
  # Confusion Matrix
  pred_class <- ifelse(prob > 0.5, 1, 0)
  cm <- table(Predicted = pred_class, Actual = actual)
  
  # AUC
  auc_val <- auc(actual, prob)
  
  # 기록
  performance_table <- rbind(performance_table, data.frame(
    Context = ctx,
    AUC = as.numeric(auc_val),
    Predicted_Yes = sum(pred_class == 1),
    Actual_Yes = sum(actual == 1)
  ))
  
  # 콘솔 출력
  print(table(actual))
  print(summary(prob))
  print(cm)
  cat("AUC =", auc_val, "\n")
}

cat("\n========== Step1 성능 평가 종료 ==========\n")


# ==============================================================================
# Export Section
# ==============================================================================

dir.create("Step1_results", showWarnings = FALSE)
dir.create("Step1_results/feature_importance", showWarnings = FALSE)

# 1) 성능표 저장
write.csv(performance_table,
          "Step1_results/Step1_performance_summary.csv",
          row.names = FALSE)

# 2) Feature Importance 저장
for (ctx in contexts) {
  
  imp <- models[[ctx]]$variable.importance
  imp_df <- data.frame(
    Feature = names(imp),
    Importance = imp
  ) %>% arrange(desc(Importance))
  
  # CSV 저장
  write.csv(imp_df,
            paste0("Step1_results/feature_importance/importance_", ctx, ".csv"),
            row.names = FALSE)
  
  # PNG 저장
  png(paste0("Step1_results/feature_importance/importance_plot_", ctx, ".png"),
      width = 900, height = 600)
  barplot(
    imp_df$Importance,
    names.arg = imp_df$Feature,
    las = 2,
    col = "steelblue",
    main = paste("Feature Importance -", ctx),
    cex.names = 0.8
  )
  dev.off()
}

# 3) 모델 저장(백업)
saveRDS(models, "Step1_results/Step1_model_backup.rds")

cat("\n===== Step1 성능 및 결과 Export 완료 =====\n")
