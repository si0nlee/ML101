# clean_and_report_missing_v2.R
# 500K Spotify 데이터:
# - 결측치 리포트
# - 타입 정리(형변환)
# - Loudness (db) 안전 변환
# - 수치 범위 sanity check
# - 중복 제거
# - 최종 clean CSV 저장

library(ggplot2)
library(dplyr)
library(tidyr)

# ---------------- 1) 데이터 로드 --------------------------------------

data <- read.csv(
  "spotify_dataset_.csv",       # ← 파일 이름 확인해서 필요하면 수정
  stringsAsFactors = FALSE,
  check.names = FALSE,          # 원래 컬럼명 그대로 사용 (공백/괄호 유지)
  na.strings = c("", "NA")      # 빈칸 / 문자열 "NA"를 NA로 처리
)

cat("===== [1] 원본 데이터 크기 =====\n")
cat("행:", nrow(data), " / 열:", ncol(data), "\n\n")


# ---------------- 2) 전체 컬럼 결측치 리포트 ------------------------

na_summary <- data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "na_count"
  ) %>%
  mutate(
    na_ratio = na_count / nrow(data)
  ) %>%
  arrange(desc(na_ratio))


p_miss_count <- na_summary %>%
  filter(na_count > 0) %>%
  slice_head(n = 20) %>%
  ggplot(aes(x = reorder(variable, na_count), y = na_count)) +
  geom_col() +
  coord_flip() +
  labs(title = "Missing Count (RAW) - Top 20", x = NULL, y = "NA count")

print(p_miss_count)


cat("===== [2] 변수별 결측치 요약 (상위 20개) =====\n")
print(head(na_summary, 20))

cat("\n===== 결측치가 1개 이상 있는 변수들만 보기 =====\n")
print(na_summary %>% filter(na_count > 0))


# ---------------- 3) 파생 컬럼 생성 (year, lyrics_len 등) -----------

data <- data %>%
  mutate(
    year = suppressWarnings(
      as.integer(sub(".*(\\d{4})$", "\\1", `Release Date`))
    ),
    lyrics_len = ifelse(is.na(text), NA_integer_, nchar(text)),
    title_len  = ifelse(is.na(song), NA_integer_, nchar(song))
  )

cat("\n===== [3] year 요약 =====\n")
print(summary(data$year))


# ---------------- 4) 형변환: numeric / tag / Loudness 정제 ----------

# (1) 수치형으로 쓸 오디오 변수들
numeric_cols <- c(
  "Popularity", "Energy", "Danceability", "Positiveness",
  "Speechiness", "Liveness", "Acousticness", "Instrumentalness",
  "Tempo", "Loudness (db)"
)
numeric_cols <- intersect(numeric_cols, colnames(data))

# (2) Context Tag (0/1) 컬럼들
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
tag_cols <- intersect(tag_cols, colnames(data))

# ---- 4-1. Loudness (db) 먼저 깨끗하게 숫자만 남기기 ----
if ("Loudness (db)" %in% colnames(data)) {
  cat("\n===== [4-1] Loudness (db) 원본 타입 / 예시 =====\n")
  print(str(data[,"Loudness (db)"]))
  print(head(data[,"Loudness (db)"], 5))
  
  # (Viz용) Loudness 원본을 숫자로 '시도'해본 값 (정제 전)
  loud_before_num <- suppressWarnings(as.numeric(gsub("[^0-9.\\-]+", "", gsub(",", ".", data$`Loudness (db)`))))
  
  
  data <- data %>%
    mutate(
      # 콤마 소수점일 가능성 → 점(.)으로 통일
      `Loudness (db)` = gsub(",", ".", `Loudness (db)`),
      # 숫자/부호/점 이외 문자 제거 (dB, 공백 등)
      `Loudness (db)` = gsub("[^0-9.\\-]+", "", `Loudness (db)`),
      # 최종적으로 numeric 변환
      `Loudness (db)` = suppressWarnings(as.numeric(`Loudness (db)`))
    )
  
  cat("\n===== [4-1] Loudness (db) 변환 후 요약 =====\n")
  print(summary(data$`Loudness (db)`))
  cat("NA 개수:", sum(is.na(data$`Loudness (db)`)), "\n")
}

# ---------------- [Viz] Loudness 정제 전/후 분포 ------------------------
if ("Loudness (db)" %in% colnames(data)) {
  
  # (Viz용) Loudness 원본을 숫자로 '시도'해본 값 (정제 전)
  loud_before_num <- suppressWarnings(
    as.numeric(gsub("[^0-9.\\-]+", "", gsub(",", ".", data$`Loudness (db)`)))
  )
  
  # ----- 기존 Loudness 정제 mutate -----
  
  # ---------------- [Viz] Loudness 정제 전/후 분포 ------------------------
  df_loud <- data.frame(
    stage = c(rep("before", length(loud_before_num)),
              rep("after",  length(data$`Loudness (db)`))),
    value = c(loud_before_num, data$`Loudness (db)`)
  )
  
  set.seed(42)
  df_loud_vis <- df_loud %>% slice_sample(n = min(200000, nrow(df_loud)))
  
  p_loud <- ggplot(df_loud_vis, aes(x = value)) +
    geom_histogram(bins = 80) +
    facet_wrap(~stage, scales = "free_y") +
    labs(
      title = "Loudness (db): Before vs After Cleaning",
      x = "Loudness (db)", y = "count"
    )
  
  print(p_loud)
}

# 너무 크면 보기만 좋게 샘플링 (시각화만 샘플, 데이터는 그대로)
set.seed(42)
df_loud_vis <- df_loud %>% slice_sample(n = min(200000, nrow(df_loud)))

p_loud <- ggplot(df_loud_vis, aes(x = value)) +
  geom_histogram(bins = 80) +
  facet_wrap(~stage, scales = "free_y") +
  labs(title = "Loudness (db): Before vs After Cleaning", x = "Loudness (db)", y = "count")

print(p_loud)



# Loudness는 이미 처리했으니 나머지 numeric만 across로 변환
numeric_cols_wo_loudness <- setdiff(numeric_cols, "Loudness (db)")

# ---- 4-2. 나머지 numeric / tag 형변환 ----
data <- data %>%
  mutate(
    across(all_of(numeric_cols_wo_loudness),
           ~ suppressWarnings(as.numeric(.))),
    across(all_of(tag_cols),
           ~ suppressWarnings(as.integer(.)))
  )

cat("\n===== [4-2] numeric / tag 형변환 후 NA 개수 =====\n")
print(colSums(is.na(data[numeric_cols])))


# ---------------- 5) 수치 범위 sanity check --------------------------

cat("\n===== [5] 수치 범위 sanity check =====\n")

# 0~100 범위이어야 하는 피쳐들 (문서에 명시)
range_0_100 <- c(
  "Popularity", "Energy", "Danceability", "Positiveness",
  "Speechiness", "Liveness", "Acousticness", "Instrumentalness"
)
range_0_100 <- intersect(range_0_100, colnames(data))

# 범위 밖 값 개수 확인만 (일단 drop은 안 하고 NA로 바꾸는 전략)
if (length(range_0_100) > 0) {
  out_of_range_summary <- lapply(range_0_100, function(col) {
    x <- data[[col]]
    bad_idx <- which(!is.na(x) & (x < 0 | x > 100))
    list(
      col = col,
      n_out = length(bad_idx),
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE)
    )
  })
  cat("---- 0~100 범위 피쳐들의 min/max 및 범위 밖 개수 ----\n")
  print(out_of_range_summary)
  
  # 정말 이상한 값(0~100 밖)은 NA로 처리 (극히 소수일 가능성이 큼)
  data <- data %>%
    mutate(
      across(
        all_of(range_0_100),
        ~ ifelse(!is.na(.) & (. < 0 | . > 100), NA_real_, .)
      )
    )
}


tempo_before <- if ("Tempo" %in% colnames(data)) data$Tempo else NULL

# Tempo sanity check: 일반적인 BPM 범위 [30, 260] 벗어나면 NA 처리
if ("Tempo" %in% colnames(data)) {
  x <- data$Tempo
  n_out <- sum(!is.na(x) & (x <= 0 | x > 260))
  cat("\nTempo에서 0 이하 또는 260 초과 값 개수:", n_out, "\n")
  
  data <- data %>%
    mutate(
      Tempo = ifelse(!is.na(Tempo) & (Tempo <= 0 | Tempo > 260),
                     NA_real_, Tempo)
    )
}


# ---------------- [Viz] Tempo sanity check 전/후 분포 -------------------
if (!is.null(tempo_before) && "Tempo" %in% colnames(data)) {
  df_tempo <- data.frame(
    stage = c(rep("before", length(tempo_before)),
              rep("after",  length(data$Tempo))),
    value = c(tempo_before, data$Tempo)
  )
  
  set.seed(42)
  df_tempo_vis <- df_tempo %>% slice_sample(n = min(200000, nrow(df_tempo)))
  
  p_tempo <- ggplot(df_tempo_vis, aes(x = value)) +
    geom_histogram(bins = 80) +
    facet_wrap(~stage, scales = "free_y") +
    labs(title = "Tempo: Before vs After Sanity Check", x = "Tempo (BPM)", y = "count")
  
  print(p_tempo)
}


cat("\n===== [5-최종] numeric_cols NA 개수 (sanity check 후) =====\n")
print(colSums(is.na(data[numeric_cols])))


# ---------------- 6) 모든 컬럼 기준 NA 행 제거 ----------------------

before_n <- nrow(data)

data_clean <- data %>% drop_na()

after_n <- nrow(data_clean)

cat("\n===== [6] NA 행 제거 결과 =====\n")
cat("원래 행 수:", before_n, "\n")
cat("clean 행 수:", after_n, "\n")
cat("제거된 행 수:", before_n - after_n, "\n")
cat(sprintf("전체의 %.4f 비율이 제거됨\n",
            (before_n - after_n) / before_n))

# ---------------- [Viz] drop_na()로 행 수 변화 -------------------------
df_dropna <- data.frame(
  step = c("Before drop_na", "After drop_na"),
  rows = c(before_n, after_n)
)

p_dropna <- ggplot(df_dropna, aes(x = step, y = rows)) +
  geom_col() +
  labs(title = "Row Count: Before vs After drop_na()", x = NULL, y = "n_rows")

print(p_dropna)


# ---------------- 7) 중복 제거 (Artist + song + Length 기준) --------

before_dup <- nrow(data_clean)

# 아티스트 + 곡 제목 + Length 조합이 완전히 같은 행은 중복으로 보고 제거
dup_keys <- c("Artist(s)", "song", "Length")
dup_keys <- intersect(dup_keys, colnames(data_clean))

if (length(dup_keys) > 0) {
  data_clean <- data_clean %>%
    distinct(across(all_of(dup_keys)), .keep_all = TRUE)
}

after_dup <- nrow(data_clean)


cat("\n===== [7] 중복 제거 결과 (Artist+song+Length 기준) =====\n")
cat("중복 제거 전 행 수:", before_dup, "\n")
cat("중복 제거 후 행 수:", after_dup, "\n")
cat("제거된 중복 행 수:", before_dup - after_dup, "\n")

# ---------------- [Viz] 중복 제거로 행 수 변화 -------------------------
df_dedup <- data.frame(
  step = c("Before de-dup", "After de-dup"),
  rows = c(before_dup, after_dup)
)

p_dedup <- ggplot(df_dedup, aes(x = step, y = rows)) +
  geom_col() +
  labs(title = "Row Count: Before vs After De-duplication", x = NULL, y = "n_rows")


print(p_dedup)



# ---------------- 8) 최종 clean CSV 저장 -----------------------------

write.csv(
  data_clean,
  "spotify_dataset_clean.csv",
  row.names = FALSE
)

cat("\nspotify_dataset_clean.csv 로 저장 완료\n")

cat("\n===== [8] 최종 NA 개수 (모든 컬럼) =====\n")
print(colSums(is.na(data_clean)))

