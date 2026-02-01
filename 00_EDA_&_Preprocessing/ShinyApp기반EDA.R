# app.R
# 500K+ Spotify Songs with Lyrics, Emotions, Audio Features
# Task 7 & 8: Contextual Tags 기반 분석 + 추천 Shiny App
# + 태그 기반 클러스터 시각화 (PCA / t-SNE / UMAP)

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(DT)

# ---------------- 1. 데이터 로드 & 기본 전처리 ------------------------

data_raw <- read.csv("spotify_dataset_clean.csv",
                     stringsAsFactors = FALSE,
                     check.names = FALSE)

data <- data_raw %>%
  mutate(
    year = suppressWarnings(
      as.integer(sub(".*(\\d{4})$", "\\1", `Release Date`))
    ),
    lyrics_len = nchar(text),
    title_len  = nchar(song)
  )

# 컨텍스트 태그 컬럼들 (0/1)
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

if (length(tag_cols) == 0) {
  stop("컨텍스트 태그 컬럼을 찾지 못했습니다. colnames(data)를 확인하세요.")
}

# 보기 예쁘게 표시할 이름 매핑
tag_display_names <- c(
  "Good for Party"                 = "Party",
  "Good for Work/Study"            = "Work / Study",
  "Good for Relaxation/Meditation" = "Relaxation / Meditation",
  "Good for Exercise"              = "Exercise",
  "Good for Running"               = "Running",
  "Good for Yoga/Stretching"       = "Yoga / Stretching",
  "Good for Driving"               = "Driving",
  "Good for Social Gatherings"     = "Social Gatherings",
  "Good for Morning Routine"       = "Morning Routine"
)

# 오디오/수치 피처 (태그 예측/프로파일링에 유용한 것들)
audio_vars <- c(
  "Popularity", "Energy", "Danceability", "Positiveness",
  "Speechiness", "Liveness", "Acousticness", "Instrumentalness",
  "Tempo", "Loudness (db)"
)
audio_vars <- intersect(audio_vars, colnames(data))

# emotion / Genre 선택지
emotion_choices <- data %>%
  filter(!is.na(emotion)) %>%
  count(emotion, sort = TRUE) %>%
  pull(emotion)

genre_choices <- data %>%
  filter(!is.na(Genre)) %>%
  count(Genre, sort = TRUE) %>%
  slice(1:50) %>%  # 상위 50개만 선택지
  pull(Genre)

# ---------------- 2. UI ------------------------------------------------

ui <- fluidPage(
  titlePanel("Contextual Tags 기반 음악 분석 & 추천 (500K Spotify Songs)"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("필터"),
      
      # 타겟 태그 선택 (Party / Study / Running 등)
      selectInput(
        "target_tag",
        "분석/추천 대상 태그 (Good for ~~):",
        choices = setNames(tag_cols, tag_display_names[tag_cols]),
        selected = "Good for Party"
      ),
      
      sliderInput(
        "year_range",
        "Release Year:",
        min = min(data$year, na.rm = TRUE),
        max = max(data$year, na.rm = TRUE),
        value = c(2000, max(data$year, na.rm = TRUE)),  # 현대 음악 위주
        step = 1,
        sep = ""
      ),
      
      sliderInput(
        "pop_range",
        "Popularity 범위:",
        min = min(data$Popularity, na.rm = TRUE),
        max = max(data$Popularity, na.rm = TRUE),
        value = c(0, max(data$Popularity, na.rm = TRUE)),
        step = 1
      ),
      
      selectInput(
        "emotion_filter",
        "감정(emotion) 선택 (옵션, 복수 선택 가능):",
        choices = emotion_choices,
        multiple = TRUE,
        selected = NULL
      ),
      
      selectInput(
        "genre_filter",
        "Genre (상위 50개 중 선택, 옵션):",
        choices = c("전체" = "ALL", genre_choices),
        selected = "ALL"
      ),
      
      textInput(
        "artist_search",
        "Artist 검색(포함 문자열):",
        value = ""
      ),
      
      tags$hr(),
      h4("Audio Scatter 설정"),
      selectInput(
        "x_var",
        "X축 변수:",
        choices = audio_vars,
        selected = "Danceability"
      ),
      selectInput(
        "y_var",
        "Y축 변수:",
        choices = audio_vars,
        selected = "Energy"
      ),
      checkboxInput("log_x", "X축 log 변환", FALSE),
      checkboxInput("log_y", "Y축 log 변환", FALSE),
      
      tags$hr(),
      h4("클러스터 시각화 설정"),
      selectInput(
        "dr_method",
        "차원 축소 방법:",
        choices = c("PCA", "t-SNE", "UMAP"),
        selected = "PCA"
      ),
      sliderInput(
        "cluster_sample",
        "클러스터링 샘플 수:",
        min = 1000, max = 20000, value = 5000, step = 1000
      ),
      
      tags$hr(),
      h4("추천 개수"),
      sliderInput(
        "top_n",
        "추천 곡 개수 (Top N):",
        min = 10, max = 200, value = 50, step = 10
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("1) 태그 전체 Overview",
                 plotOutput("tagOverviewPlot", height = "450px")),
        tabPanel("2) 선택 태그 프로파일 (Tag=1 vs 0)",
                 plotOutput("tagProfilePlot", height = "450px")),
        tabPanel("3) 태그 동시 출현 (Co-occurrence)",
                 plotOutput("cooccurrencePlot", height = "450px")),
        tabPanel("4) Audio Scatter (태그 중심)",
                 plotOutput("scatterPlot", height = "450px")),
        tabPanel("5) 추천 플레이리스트 (Top N)",
                 DTOutput("recommendTable")),
        tabPanel("6) 태그 클러스터 (PCA / t-SNE / UMAP)",
                 plotOutput("clusterPlot", height = "500px"))
      )
    )
  )
)

# ---------------- 3. SERVER --------------------------------------------

server <- function(input, output, session) {
  
  # 공통 필터 적용 데이터 (year / popularity / emotion / genre / artist) ----
  filtered_data <- reactive({
    df <- data
    
    # 연도
    df <- df %>%
      filter(!is.na(year),
             year >= input$year_range[1],
             year <= input$year_range[2])
    
    # Popularity
    df <- df %>%
      filter(!is.na(Popularity),
             Popularity >= input$pop_range[1],
             Popularity <= input$pop_range[2])
    
    # emotion
    if (length(input$emotion_filter) > 0) {
      df <- df %>%
        filter(emotion %in% input$emotion_filter)
    }
    
    # Genre
    if (input$genre_filter != "ALL") {
      df <- df %>%
        filter(Genre == input$genre_filter)
    }
    
    # Artist 검색
    if (input$artist_search != "") {
      df <- df %>%
        filter(str_detect(
          tolower(`Artist(s)`),
          tolower(input$artist_search)
        ))
    }
    
    df
  })
  
  # 샘플링 (그래프용 데이터가 너무 크면 50,000곡만 사용) -------------
  plot_sample <- reactive({
    df <- filtered_data()
    if (nrow(df) > 50000) {
      df %>% sample_n(50000)
    } else {
      df
    }
  })
  
  # 3-1. 태그 전체 Overview: 각 태그의 비율 ----------------------------
  output$tagOverviewPlot <- renderPlot({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    tag_stats <- df %>%
      summarise(across(all_of(tag_cols),
                       ~ mean(. == 1, na.rm = TRUE))) %>%
      pivot_longer(cols = everything(),
                   names_to = "tag",
                   values_to = "ratio") %>%
      mutate(
        tag_display = tag_display_names[tag],
        ratio = ifelse(is.na(ratio), 0, ratio)
      )
    
    ggplot(tag_stats,
           aes(x = reorder(tag_display, ratio), y = ratio)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      theme_minimal(base_size = 14) +
      labs(
        title = "전체 곡 중 각 Context Tag 비율 (필터 반영)",
        x = "Tag (Good for ~~)",
        y = "비율"
      )
  })
  
  # 3-2. 선택 태그 프로파일: Tag=1 vs Tag=0 에서 Audio Feature 분포 ----
  output$tagProfilePlot <- renderPlot({
    df <- plot_sample()
    req(nrow(df) > 0)
    
    target_tag <- input$target_tag
    target_name <- tag_display_names[target_tag]
    
    # Tag 상태 변수 생성
    df <- df %>%
      mutate(
        tag_status = ifelse(.data[[target_tag]] == 1,
                            paste0(target_name, " = 1"),
                            paste0(target_name, " = 0"))
      ) %>%
      filter(!is.na(tag_status))
    
    # Audio feature들을 long 형태로 변환
    df_long <- df %>%
      pivot_longer(cols = all_of(audio_vars),
                   names_to = "feature",
                   values_to = "value") %>%
      filter(!is.na(value))
    
    ggplot(df_long,
           aes(x = feature, y = value, fill = tag_status)) +
      geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
      coord_flip() +
      theme_minimal(base_size = 13) +
      labs(
        title = paste0("[", target_name, "] 태그 여부에 따른 Audio Feature 분포"),
        x = "Audio Feature",
        y = "값"
      ) +
      scale_fill_discrete(name = "Tag 상태")
  })
  
  # 3-3. 태그 동시 출현(Co-occurrence): 선택 태그=1인 곡들에서 다른 태그 비율
  output$cooccurrencePlot <- renderPlot({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    target_tag <- input$target_tag
    target_name <- tag_display_names[target_tag]
    
    df_tag1 <- df %>%
      filter(.data[[target_tag]] == 1)
    
    req(nrow(df_tag1) > 0)
    
    cooc <- df_tag1 %>%
      summarise(across(all_of(tag_cols),
                       ~ mean(. == 1, na.rm = TRUE))) %>%
      pivot_longer(cols = everything(),
                   names_to = "tag",
                   values_to = "ratio") %>%
      filter(tag != target_tag) %>%
      mutate(
        tag_display = tag_display_names[tag],
        ratio = ifelse(is.na(ratio), 0, ratio)
      ) %>%
      arrange(desc(ratio))
    
    ggplot(cooc,
           aes(x = reorder(tag_display, ratio), y = ratio)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      theme_minimal(base_size = 14) +
      labs(
        title = paste0("[", target_name, "] = 1인 곡들에서 다른 Tag 동시 출현 비율"),
        x = "다른 Tag",
        y = "동시 출현 비율"
      )
  })
  
  # 3-4. Audio Scatter: 선택 태그 기준으로 색칠 ------------------------
  output$scatterPlot <- renderPlot({
    df <- plot_sample()
    req(nrow(df) > 0)
    
    target_tag <- input$target_tag
    target_name <- tag_display_names[target_tag]
    x_var <- input$x_var
    y_var <- input$y_var
    
    df <- df %>%
      mutate(
        tag_status = ifelse(.data[[target_tag]] == 1,
                            paste0(target_name, " = 1"),
                            paste0(target_name, " = 0"))
      ) %>%
      filter(
        !is.na(tag_status),
        !is.na(.data[[x_var]]),
        !is.na(.data[[y_var]])
      )
    
    if (input$log_x) {
      df <- df %>% filter(.data[[x_var]] > 0)
    }
    if (input$log_y) {
      df <- df %>% filter(.data[[y_var]] > 0)
    }
    
    req(nrow(df) > 0)
    
    p <- ggplot(df,
                aes(x = .data[[x_var]], y = .data[[y_var]],
                    color = tag_status)) +
      geom_point(alpha = 0.4, size = 1) +
      theme_minimal(base_size = 14) +
      labs(
        title = paste0("[", target_name, "] 기준 Audio Feature Scatter"),
        x = x_var,
        y = y_var,
        color = "Tag 상태"
      )
    
    if (input$log_x) p <- p + scale_x_log10()
    if (input$log_y) p <- p + scale_y_log10()
    
    p
  })
  
  # 3-5. 추천 플레이리스트: target_tag = 1 & 필터 반영, Popularity 기준 상위 N
  output$recommendTable <- renderDT({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    target_tag <- input$target_tag
    target_name <- tag_display_names[target_tag]
    
    df_rec <- df %>%
      filter(.data[[target_tag]] == 1) %>%
      arrange(desc(Popularity)) %>%
      mutate(
        lyrics_head = ifelse(
          nchar(text) > 120,
          paste0(substr(text, 1, 120), "..."),
          text
        )
      ) %>%
      select(
        `Artist(s)`, song, Genre, emotion, year,
        Popularity, Energy, Danceability, Positiveness,
        all_of(tag_cols),
        lyrics_head
      )
    
    if (nrow(df_rec) > input$top_n) {
      df_rec <- df_rec[1:input$top_n, ]
    }
    
    datatable(
      df_rec,
      options = list(pageLength = 20, autoWidth = TRUE),
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left;",
        paste0("추천 결과: [", target_name, "] = 1 & 필터 반영, Popularity 상위 ",
               min(input$top_n, nrow(df_rec)), "곡")
      )
    )
  })
  
  # ---- 3-6. 태그 클러스터 시각화 (PCA / t-SNE / UMAP) ----------------
  
  cluster_sample <- reactive({
    df <- filtered_data()
    
    # audio_vars 중 NA가 있는 행 제거 (차원 축소 안정성을 위해)
    df <- df %>%
      filter(if_all(all_of(audio_vars), ~ !is.na(.)))
    
    # 샘플 수 제한
    n <- nrow(df)
    if (n == 0) return(df)
    
    target_n <- input$cluster_sample
    if (n > target_n) {
      df %>% sample_n(target_n)
    } else {
      df
    }
  })
  
  output$clusterPlot <- renderPlot({
    df <- cluster_sample()
    req(nrow(df) > 10)  # 최소 몇 개 이상 있어야 그림 의미 있음
    
    target_tag  <- input$target_tag
    target_name <- tag_display_names[target_tag]
    method      <- input$dr_method
    
    # 차원 축소용 행렬
    X <- df %>%
      select(all_of(audio_vars)) %>%
      as.matrix()
    
    # scale (중심화 + 표준화)
    X_scaled <- scale(X)
    
    # ---- 차원 축소 ----
    if (method == "PCA") {
      dr <- prcomp(X_scaled, center = FALSE, scale. = FALSE)
      coords <- dr$x[, 1:2]
    } else if (method == "t-SNE") {
      if (!requireNamespace("Rtsne", quietly = TRUE)) {
        validate(
          need(FALSE,
               "패키지 'Rtsne'가 설치되어 있지 않습니다. install.packages('Rtsne') 후 다시 시도하세요.")
        )
      }
      tsne_res <- Rtsne::Rtsne(
        X_scaled,
        dims = 2,
        perplexity = 30,
        verbose = FALSE,
        check_duplicates = FALSE
      )
      coords <- tsne_res$Y
    } else if (method == "UMAP") {
      if (!requireNamespace("umap", quietly = TRUE)) {
        validate(
          need(FALSE,
               "패키지 'umap'이 설치되어 있지 않습니다. install.packages('umap') 후 다시 시도하세요.")
        )
      }
      umap_res <- umap::umap(X_scaled)
      coords <- umap_res$layout
    } else {
      validate(need(FALSE, "지원하지 않는 차원 축소 방법입니다."))
    }
    
    df$Dim1 <- coords[, 1]
    df$Dim2 <- coords[, 2]
    
    df <- df %>%
      mutate(
        tag_status = ifelse(.data[[target_tag]] == 1,
                            paste0(target_name, " = 1"),
                            paste0(target_name, " = 0"))
      )
    
    ggplot(df, aes(x = Dim1, y = Dim2, color = tag_status)) +
      geom_point(alpha = 0.5, size = 1) +
      theme_minimal(base_size = 14) +
      labs(
        title = paste0("[", target_name, "] 태그 기반 클러스터 시각화 (", method, ")"),
        x = "Dim 1",
        y = "Dim 2",
        color = "Tag 상태"
      )
  })
}

# ---------------- 4. 앱 실행 ------------------------------------------
  
shinyApp(ui = ui, server = server)
