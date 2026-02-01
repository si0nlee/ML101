library(dplyr)

# 1) 다시 로드 (공백 → NA)
data <- read.csv(
  "spotify_dataset_.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("", "NA")
)


# 2) 연도, 길이 등 편의 컬럼 만들기 -----------------------------------
data <- data %>%
  mutate(
    year = suppressWarnings(
      as.integer(sub(".*(\\d{4})$", "\\1", `Release Date`))
    ),
    lyrics_len = nchar(text),
    title_len  = nchar(song)
  )

# 3) Task7/8 핵심 피처 목록 --------------------------------------------

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
  "emotion", "Genre", "text", "Artist(s)", "song", "year"
)

# 실제 존재하는 컬럼만 남기기 (안전장치)
core_cols <- intersect(core_cols, colnames(data))

# 4) Album/Time signature 같이 부차적인 건 간단히 채우기 ---------------

data <- data %>%
  mutate(
    Album          = ifelse(is.na(Album), "Unknown Album", Album),
    `Time signature` = ifelse(is.na(`Time signature`),
                              "4/4", `Time signature`)  # 가장 흔한 값으로 대체
  )

# 5) 핵심 피처에 NA 있는 행은 제거 ------------------------------------

before_n <- nrow(data)

data_clean <- data %>%
  tidyr::drop_na(all_of(core_cols))

after_n <- nrow(data_clean)

cat("원래 행 수:", before_n, "\n")
cat("clean 데이터 행 수:", after_n, "\n")
cat("제거된 행 수:", before_n - after_n, "\n")

# 6) Shiny / 모델링용 CSV 저장 ----------------------------------------

write.csv(data_clean,
          "spotify_dataset_clean.csv",
          row.names = FALSE)

cat("spotify_dataset_clean.csv 로 저장 완료\n")

