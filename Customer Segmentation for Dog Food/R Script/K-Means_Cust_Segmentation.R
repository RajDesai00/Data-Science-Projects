#Importing the dataset:
cust_seg = read.csv('D:/Sem_2/Marketing Analytics/Assignments/Cust_Segmentation.csv')
View(cust_seg)
colnames(cust_seg)

#Renaming the columns with distorted headers:
library(dplyr)
cust_seg <- cust_seg %>% rename(Respondent = ï..Respondent, Age = Age..Int.)
colnames(cust_seg)

#Initial analysis using EDA method:
library(ggplot2)
ggplot(cust_seg, aes(x = Respondent)) + 
  geom_line(aes(y = S1), color = 'darkred', size = 1.5) + 
  geom_line(aes(y = S2), color = 'steelblue', size = 1.3) +
  geom_line(aes(y = S3), color = 'Black', size = 1.1) +
  geom_line(aes(y = S4), color = 'Green', size = 1) +
  geom_line(aes(y = S5), color = 'orange', size = 0.8) +
  geom_line(aes(y = S6), color = 'magenta', size = 0.6) +
  ggtitle('Response on Likert Scale') +
  xlab('Respondent') +
  ylab('Responses')

ggplot(cust_seg) +
  geom_bar(mapping = aes(x = AgeCat)) +
  ggtitle('COUNT OF RESPONDENT WITHIN DIFFERENT AGE CATEGORY') +
  xlab('Age') +
  ylab('Count')

ggplot(cust_seg) +
  geom_bar(mapping = aes(x = Gender)) +
  ggtitle('COUNT OF RESPONDENT WITH RESPECT TO GENDER') +
  xlab('Gender') +
  ylab('Count')

#Changing the class from character to factor
sapply(cust_seg, class)
cust_seg_1 <- cust_seg
cust_seg_1$AgeCat <- as.factor(cust_seg_1$AgeCat)
cust_seg_1$Gender <- as.factor(cust_seg_1$Gender)
sapply(cust_seg_1, class)
View(cust_seg_1)

#Label encoding the gender column:
cust_seg_2 <- cust_seg_1
cust_seg_2$Gender <- as.integer(as.factor(cust_seg_2$Gender))
cust_seg_2$AgeCat <- as.integer(as.factor(cust_seg_2$AgeCat))
View(cust_seg_2)

#Dropping unnecessary columns:
cust_seg_2 <- cust_seg_2[c("S1","S2","S3","S4","S5","S6","Age","AgeCat","Gender")]
#Plotting the distance of all the variables from each other for selecting centroids:
library(tidyverse)
library(cluster)
library(factoextra)
distance <- get_dist(cust_seg_2)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#Finding the number of clusters required using Elbow Method:
wssplot <- function(data, nc=15, seed=123){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of groups",
       ylab="Sum of squares within a group")
}

wssplot(cust_seg_2, nc = 20)

#Applying k-Means for generating clusters:
set.seed(123)
clustering <- kmeans(cust_seg_2, centers = 4, nstart = 20)
clustering

#Accessing the cluster details:
clustering$cluster
clustering$size
centers <- clustering$centers

#Animating the iteration from K-Means clustering process
library(animation)
set.seed(123)
kmeans.ani(cust_seg_2, 4)

#Validating the clusters generated using silhouette coefficient:
sil <- silhouette(clustering$cluster, dist(cust_seg_2))
fviz_silhouette(sil)

#Visualizing the clusters:
fviz_cluster(clustering, data = cust_seg_2,
             palette = c("Red", "Green", "Black", "Blue"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

#Interpreting the clusters:
df <- data.frame(clustering$cluster)
colnames(df)
df <- df %>% rename(Cluster_no = clustering.cluster)
view(df)
clustered_df <- cbind(cust_seg_1[2:10], df)
View(clustered_df)
library(dplyr)
cluster_1 <- filter(clustered_df, Cluster_no == 1)
cluster_2 <- filter(clustered_df, Cluster_no == 2)
cluster_3 <- filter(clustered_df, Cluster_no == 3)
cluster_4 <- filter(clustered_df, Cluster_no == 4)
