# ML101  

  
# 🎧 Spotify Song Recommender: Hybrid Strategy Analysis  
Kaggle의 '500K+ Spotify Songs' 데이터를 활용한 개인화 음악 추천 시스템 연구  

본 프로젝트는 스포티파이 곡들의 감정(Emotions), 가사(Lyrics) 및 오디오 특징 데이터를 분석하여 사용자의 상황에 맞는 곡을 추천하는 최적의 모델을 탐색합니다. 1차 단일 모델(Single Model) 분석부터 2차 다중 모델(Multi Model) 확장 및 실험까지의 과정을 담고 있습니다.  

  
# 📊 Dataset  
Source: Kaggle - 500K+ Spotify Songs with Lyrics, Emotions & More  

Description: 50만 곡 이상의 곡에 대한 오디오 특징(Energy, Valence, Danceability 등)과 가사, 감정 라벨링이 포함된 데이터셋입니다.  

  
# 🛠 Tech Stack  
Language: R (R을 활용한 통계 분석 및 모델링)  
Library: ggplot2, dplyr, caret, tidyverse 등  
  

# 📂 Repository Structure  
**1️⃣ Phase 1: Single Model Analysis**  
단일 모델을 통해 변수 간 상관관계를 파악하고 추천의 기초가 되는 확률 분포를 분석했습니다.  

1) Step1_확률분포 & results: 곡 특징별 분포 및 기초 분석 결과  
2) correlation_results: 변수 간 상관관계 분석  
3) feature_importance_FINAL: 추천 모델의 핵심 변수 중요도 산출  
4) step1+2+3.R: 추천 알고리즘 단계별 통합 구현 코드  
5) playlist_plots_FINAL: 생성된 플레이리스트 시각화 결과물  

**2️⃣ Phase 2: Multi Model & Optimization**  
다양한 지표를 도입하고 실험을 통해 모델을 고도화했습니다.  

Baseline Study:  
1) baseline(vectorized).R: 벡터화된 기준 모델 구현  
2) plot_context_correlation_heatmap.pdf: 컨텍스트 간 상관관계 분석 히트맵  

  
Additional Experiments:  
1) Top-N: 추천 후보군 최적화 실험  
2) Similarity metric: 곡 간 유사도 측정 방식 변경 실험  
3) Re-rank (Artist penalty): 추천 결과의 다양성을 위해 동일 아티스트 중복 노출을 제한하는 리랭킹 알고리즘 적용  

  
# 🚀 Key Insights  
Feature Importance: 음악의 감정 수치와 오디오 특징 중 어떤 요소가 플레이리스트 구성에 가장 큰 영향을 미치는지 규명했습니다.  
Algorithm Optimization: 단순 추천에서 나아가 Artist Penalty 기법을 도입하여 추천의 다양성을 확보하고 사용자 만족도를 높였습니다.  
Visual Analysis: 상관관계 히트맵 및 스캐터 플롯을 통해 모델의 타당성을 시각적으로 검증했습니다.  

