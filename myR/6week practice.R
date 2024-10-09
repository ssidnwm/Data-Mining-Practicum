library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(ggplot2)

# k-means clust
set.seed(2018)
synth.data <- data.frame(
  x1 = c(rnorm(20, 3, 1.5), rnorm(20, 0, 1), rnorm(20, 5, 1)),
  x2 = c(rnorm(20, 0, 1), rnorm(20, 4, 1), rnorm(20, 5, 1))
)

ndata <- nrow(synth.data)

# 유클리드 거리 계산 함수
u_dist <- function(u, v) {
  sqrt(sum((u - v)^2))
}

# 클러스터 수 설정
k <- 3

# 초기 중심점 설정
set.seed(123)  # 재현성을 위해 시드 설정
cents <- data.frame(cl = 1:k)
cents <- cbind(cents, synth.data[sample(1:ndata, k),])

# 각 데이터 포인트를 가장 가까운 중심점에 할당하는 함수
assign_clusters <- function(data, cents) {
  apply(data[, c("x1", "x2")], 1, function(point) {
    dists <- apply(cents[, c("x1", "x2")], 1, function(centroid) {
      u_dist(point, centroid)
    })
    which.min(dists)
  })
}

# 중심점을 업데이트하는 함수
update_centroids <- function(data, k) {
  cents_updated <- data %>%
    group_by(cl) %>%
    summarise(x1 = mean(x1), x2 = mean(x2)) %>%
    ungroup()
  
  # 클러스터 수가 k와 동일하지 않으면, NA로 처리된 값을 삭제하고 새로운 중심점을 추가
  while (nrow(cents_updated) < k) {
    new_cents <- synth.data[sample(1:ndata, 1), ]
    new_cent <- data.frame(cl = max(cents_updated$cl) + 1, x1 = new_cents$x1, x2 = new_cents$x2)
    cents_updated <- rbind(cents_updated, new_cent)
  }
  
  return(cents_updated)
}

# K-means 클러스터링 실행
for (i in 1:10) {  # 10번 반복
  # 1. 각 데이터 포인트를 가장 가까운 중심점에 할당
  synth.data$cl <- factor(assign_clusters(synth.data, cents), levels = 1:k)
  
  # 2. 중심점을 할당된 데이터의 평균으로 업데이트
  cents <- update_centroids(synth.data, k)
}

# 결과 시각화 (세 가지 다른 색상을 지정)
synth.data %>%
  ggplot(aes(x = x1, y = x2, col = cl)) +
  geom_point(shape = 1, size = 2) +  # 데이터 포인트
  theme_bw() +
  geom_point(data = cents, aes(x = x1, y = x2), shape = 4, col = 'black', size = 4) +  # 중심점
  scale_color_manual(values = c("red", "green", "blue")) +  # 세 가지 색상 지정
  labs(title = "K-means Clustering with Synthesized Data", x = "x1", y = "x2") +
  theme(legend.position = "right")


#프로틴 가지고
protein <- read.table("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 마이닝 실습/Data-Mining-Practicum/myR/protein.txt",
                      sep = "\t", header = TRUE)
vars.to.use <- colnames(protein)[-1]
pmatrix <- scale(protein[,vars.to.use])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

d <- dist(pmatrix, method = "euclidean")
pfit <- hclust(d, method = "ward.D")
plot(pfit, labels=protein$Country)

groups <- cutree(pfit, k = 5)
print_clusters<- function(labels, k){
  for(i in 1:k){
    print(paste("cluster",i))
    print(protein[labels == i, c("Country","RedMeat","Fish","Fr.Veg")])
  }
}
print_clusters(groups,5)


pclusters <- kmeans(pmatrix, 5, nstart = 100, iter.max = 100)
groups <- pclusters$cluster
library(factoextra) 
install.packages("factoextra")
library(fpc)
install.packages("fpc")
library(ggplot2)
# K값 범위 설정
k_values <- 2:10  # 클러스터 수 K를 2부터 10까지 실험
wss_values <- numeric(length(k_values))  # WSS 저장 리스트
ch_values <- numeric(length(k_values))   # CH index 저장 리스트

# K값별로 K-means 클러스터링을 실행하고 WSS와 CH index 계산
for (i in 1:length(k_values)) {
  k <- k_values[i]
  kmeans_result <- kmeans(pmatrix, centers = k, nstart = 25)
  
  # WSS 값 저장
  wss_values[i] <- kmeans_result$tot.withinss
  
  # CH Index 계산 (fpc 패키지 사용)
  stats <- cluster.stats(d = dist(pmatrix), kmeans_result$cluster)
  ch_values[i] <- stats$ch  # Calinski-Harabasz Index
}

# WSS 그래프 그리기
wss_df <- data.frame(K = k_values, WSS = wss_values)
ggplot(wss_df, aes(x = K, y = WSS)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  ggtitle("Within-Cluster Sum of Squares (WSS) vs. K") +
  xlab("Number of Clusters (K)") +
  ylab("WSS")

# CH Index 그래프 그리기
ch_df <- data.frame(K = k_values, CH = ch_values)
ggplot(ch_df, aes(x = K, y = CH)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  ggtitle("Calinski-Harabasz Index vs. K") +
  xlab("Number of Clusters (K)") +
  ylab("Calinski-Harabasz Index")

#Euclidean Distance 외에 다른 종류의 다양한 distance measure (4개 이상이상)을 조사하시오조사하시오
#멘헤탄
manhattan_dist <- dist(pmatrix, method = "manhattan")
print(manhattan_dist)
#맨해튼 kmeans
# Manhattan Distance 계산 함수 정의
manhattan_dist <- function(u, v) {
  sum(abs(u - v))
}

# 각 데이터 포인트를 가장 가까운 중심점에 할당하는 함수 (Manhattan Distance 사용)
assign_clusters_manhattan <- function(data, centers) {
  apply(data, 1, function(point) {
    distances <- apply(centers, 1, function(center) {
      manhattan_dist(point, center)
    })
    which.min(distances)  # 가장 가까운 중심점의 클러스터 번호 반환
  })
}

# 중심점을 업데이트하는 함수
update_centers <- function(data, clusters, k) {
  centers <- matrix(0, nrow = k, ncol = ncol(data))
  for (i in 1:k) {
    centers[i, ] <- colMeans(data[clusters == i, , drop = FALSE])  # 각 클러스터 내의 데이터 평균 계산
  }
  centers
}

# K-means 클러스터링 실행 (Manhattan Distance 기반)
kmeans_manhattan <- function(data, k, max_iter = 100) {
  set.seed(123)
  centers <- data[sample(1:nrow(data), k), ]  # 초기 중심점 무작위 선택
  clusters <- assign_clusters_manhattan(data, centers)  # 초기 클러스터 할당
  
  for (i in 1:max_iter) {
    centers <- update_centers(data, clusters, k)  # 중심점 업데이트
    new_clusters <- assign_clusters_manhattan(data, centers)  # 클러스터 다시 할당
    
    if (all(new_clusters == clusters)) {
      break  # 클러스터가 변하지 않으면 종료
    }
    clusters <- new_clusters
  }
  
  list(clusters = clusters, centers = centers)
}

# 클러스터링 실행 (K = 3으로 설정)
k <- 3
kmeans_result_manhattan <- kmeans_manhattan(pmatrix, k)

# 클러스터링 결과 시각화
clusters <- kmeans_result_manhattan$clusters
centers <- kmeans_result_manhattan$centers
#10개의 차원을 시각화하기 위하여 2개의 차원으로 축소
pca_result <- prcomp(pmatrix, scale = TRUE)
pca_data <- as.data.frame(pca_result$x[, 1:2])
# 클러스터 정보를 PCA 데이터에 추가
pca_data$cluster_manhattan <- as.factor(kmeans_result_manhattan$clusters)

# PCA 결과를 사용한 시각화
ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster_manhattan)) +
  geom_point(size = 3) +
  ggtitle("K-means Clustering with Manhattan Distance (PCA)") +
  theme_minimal()
# 시각화 (첫 번째 두 개의 차원으로만 시각화)
protein$cluster_manhattan <- as.factor(clusters)
ggplot(protein, aes(x = RedMeat, y = Fish, color = cluster_manhattan)) +
  geom_point(size = 3) +
  geom_point(data = as.data.frame(centers), aes(x = V1, y = V2), color = 'black', size = 5, shape = 4) +
  ggtitle("K-means Clustering with Manhattan Distance") +
  theme_minimal()
ggplot(protein, aes(x = RedMeat, y = WhiteMeat, color = cluster_manhattan)) +
  geom_point(size = 3) +
  geom_point(data = as.data.frame(centers), aes(x = V1, y = V2), color = 'black', size = 5, shape = 4) +
  ggtitle("K-means Clustering with Manhattan Distance") +
  theme_minimal()
ggplot(protein, aes(x = RedMeat, y = Eggs, color = cluster_manhattan)) +
  geom_point(size = 3) +
  geom_point(data = as.data.frame(centers), aes(x = V1, y = V2), color = 'black', size = 5, shape = 4) +
  ggtitle("K-means Clustering with Manhattan Distance") +
  theme_minimal()
ggplot(protein, aes(x = RedMeat, y = Cereals, color = cluster_manhattan)) +
  geom_point(size = 3) +
  geom_point(data = as.data.frame(centers), aes(x = V1, y = V2), color = 'black', size = 5, shape = 4) +
  ggtitle("K-means Clustering with Manhattan Distance") +
  theme_minimal()
ggplot(protein, aes(x = RedMeat, y = Starch, color = cluster_manhattan)) +
  geom_point(size = 3) +
  geom_point(data = as.data.frame(centers), aes(x = V1, y = V2), color = 'black', size = 5, shape = 4) +
  ggtitle("K-means Clustering with Manhattan Distance") +
  theme_minimal()
ggplot(protein, aes(x = RedMeat, y = Nuts, color = cluster_manhattan)) +
  geom_point(size = 3) +
  geom_point(data = as.data.frame(centers), aes(x = V1, y = V2), color = 'black', size = 5, shape = 4) +
  ggtitle("K-means Clustering with Manhattan Distance") +
  theme_minimal()
ggplot(protein, aes(x = WhiteMeat, y = Cereals, color = cluster_manhattan)) +
  geom_point(size = 3) +
  geom_point(data = as.data.frame(centers), aes(x = V1, y = V2), color = 'black', size = 5, shape = 4) +
  ggtitle("K-means Clustering with Manhattan Distance") +
  theme_minimal()
print(centers)  # 중심점의 좌표 출력

## 코사인 유사도

# 데이터 준비 (표준화된 pmatrix 사용)
pmatrix <- scale(protein[, -1])  # 첫 번째 열 제외하고 데이터 표준화

# 코사인 유사도 계산 함수 정의
cosine_similarity <- function(x, y) {
  sum(x * y) / (sqrt(sum(x^2)) * sqrt(sum(y^2)))
}

# 코사인 거리를 계산하는 함수 (1 - 코사인 유사도)
cosine_distance <- function(data) {
  n <- nrow(data)
  dist_matrix <- matrix(0, n, n)  # 거리 행렬 초기화
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        dist_matrix[i, j] <- 1 - cosine_similarity(data[i, ], data[j, ])  # 1 - 유사도 = 거리
      }
    }
  }
  
  as.dist(dist_matrix)  # 거리 행렬 반환
}

# 코사인 거리를 사용한 K-means 클러스터링 실행 함수
kmeans_cosine <- function(data, k, max_iter = 100) {
  set.seed(123)
  centers <- data[sample(1:nrow(data), k), ]  # 초기 중심점 무작위 선택
  clusters <- rep(0, nrow(data))  # 클러스터 할당 초기화
  
  for (i in 1:max_iter) {
    # 각 데이터 포인트를 가장 가까운 중심점에 할당
    dist_matrix <- cosine_distance(rbind(centers, data))
    dist_matrix <- as.matrix(dist_matrix)[(k+1):(k+nrow(data)), 1:k]
    new_clusters <- apply(dist_matrix, 1, which.min)  # 가장 가까운 중심점 찾기
    
    if (all(new_clusters == clusters)) {
      break  # 클러스터가 변하지 않으면 종료
    }
    
    clusters <- new_clusters
    for (j in 1:k) {
      centers[j, ] <- colMeans(data[clusters == j, , drop = FALSE])  # 중심점 업데이트
    }
  }
  
  list(clusters = clusters, centers = centers)
}

# 클러스터링 실행 (K = 3으로 설정)
k <- 3
kmeans_result_cosine <- kmeans_cosine(pmatrix, k)

# 클러스터링 결과 시각화 준비

centers_cosine <- kmeans_result_cosine$centers  # 중심점 가져오기
clusters_cosine <- kmeans_result_cosine$clusters  # 클러스터 할당 가져오기

# PCA 수행
pca_result <- prcomp(pmatrix, scale. = TRUE)  # PCA 수행
pca_data <- as.data.frame(pca_result$x[, 1:2])  # 첫 번째 두 개의 주성분 (PC1, PC2)만 추출

# 클러스터링 결과를 PCA 데이터에 추가
pca_data$cluster_cosine <- as.factor(clusters_cosine)

# PCA 결과를 사용한 시각화
ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster_cosine)) +
  geom_point(size = 3) +
  ggtitle("K-means Clustering with Cosine Distance (PCA)") +
  theme_minimal()

#DBScan
install.packages("dbscan")
library(dbscan)

# 데이터 준비 (protein 데이터를 사용한다고 가정)
pmatrix <- scale(protein[, -1])  # 첫 번째 열 제외하고 데이터 표준화
summary(protein)
str(pmatrix)  # pmatrix의 구조 확인
pmatrix <- scale(protein[, 2:9])  # 수치형 열만 선택

# DBSCAN 클러스터링 수행
distance_matrix <- dist(pmatrix, method = "euclidean")
# eps: 거리 기준, minPts: 최소 이웃 포인트 수
dbscan_result <- dbscan(as.matrix(distance_matrix), eps = 5.0, minPts = 3)

# 클러스터 결과 확인
dbscan_result$cluster

# DBSCAN 결과를 protein 데이터에 추가
protein$cluster_dbscan <- as.factor(dbscan_result$cluster)

# PCA 수행
pca_result <- prcomp(pmatrix, scale. = TRUE)  # PCA 수행
pca_data <- as.data.frame(pca_result$x[, 1:2])  # 첫 번째 두 개의 주성분 (PC1, PC2)만 추출

# 클러스터링 결과를 PCA 데이터에 추가
pca_data$cluster_dbscan <- as.factor(dbscan_result$cluster)

# PCA 결과를 사용한 시각화
ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster_dbscan)) +
  geom_point(size = 3) +
  ggtitle("DBSCAN Clustering with PCA") +
  theme_minimal()
