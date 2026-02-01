library(dplyr)
library(tidyr)

# 1) CSV 로드할 때 공백("")도 NA 로 인식
data <- read.csv(
  "spotify_dataset_.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("", "NA")     # ★ 중요: 빈칸/문자 "NA"를 NA로 처리
)

cat("행:", nrow(data), " / 열:", ncol(data), "\n\n")

# 2) 변수별 결측치 개수/비율 ------------------------------------------------
na_summary <- data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "na_count") %>%
  mutate(na_ratio = na_count / nrow(data)) %>%
  arrange(desc(na_ratio))

cat("===== 변수별 결측치 요약 (상위 20개) =====\n")
print(head(na_summary, 20))

# 3) Task 7/8에서 쓸 핵심 피처들의 결측치만 따로 보기 -------------------

audio_cols <- c(
  "Popularity", "Energy", "Danceability", "Positiveness",
  "Speechiness", "Liveness", "Acousticness", "Instrumentalness",
  "Tempo", "Loudness (db)"
)

tag_cols <- c(
  "Good for Party",
  "Good for Work/Study",
  "Good for Relaxation/Meditation",
  "Good for Exercise",
  "Good for Running",
  "Good for Yoga/Stretching",
  "Good for Driving",
  "Good for Social Gatherings",
  "Good for Morning Routine"
)

core_cols <- c(
  audio_cols,
  tag_cols,
  "emotion", "Genre", "text", "Artist(s)", "song"
)

audio_na <- na_summary %>% filter(variable %in% audio_cols)
tag_na   <- na_summary %>% filter(variable %in% tag_cols)
core_na  <- na_summary %>% filter(variable %in% core_cols)

cat("\n===== 오디오 Feature 결측치 =====\n")
print(audio_na)

cat("\n===== Context Tag 결측치 =====\n")
print(tag_na)

cat("\n===== 핵심 피처(모델용) 결측치 =====\n")
print(core_na)
