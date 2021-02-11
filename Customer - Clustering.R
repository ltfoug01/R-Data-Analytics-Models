##Problem Description----------------------------------------------------------
'
 We use Kmeans clustering to discover group membership for an unsupervised 
 problem with no response variable.
'

library(caret)

##Summary Data-----------------------------------------------------------------
seg.df <- read.csv('customer_data.csv')

head(seg.df)

#convert character to factor for clustering algorithms
seg.df$gender <- as.factor(seg.df$gender)
seg.df$ownHome <- as.factor(seg.df$ownHome)
seg.df$subscribe <- as.factor(seg.df$subscribe)

summary(seg.df)


##Hierarchical Clustering------------------------------------------------------
'
 Using hierarchical clustering, cluster the data. Cut it into some number of 
 segments, and visualize those. We use the daisy function to computes the 
 distance between each pair of observations in the dataset; this function can 
 also include factor variables.
'

library(cluster)  

ex.dist <- daisy(seg.df)   #compute the distance btw each pair of observations
ex.hc   <- hclust(ex.dist) #hierarchical cluster of the distances

plot(ex.hc)                #Plot dendrogram without splitting into groups


rect.hclust(ex.hc, k = 4, border = 'red') #hclust's proposal for 4 groups/clusters


seg.ex.segment <- cutree(ex.hc, k = 4)    #Save the group for each observation


table(seg.ex.segment) #membership vector for 4 groups/clusters

'
 Next we draw a table of the means of each cluster to inspect the segmentation 
 results. The aggregate function aggregates numerical variables by a give 
 categorical variable. Here we specify the formula of the form: numerical ~ 
 categorical. The "." means agregate all variables by the segmentation group.
'

aggregate(. ~ seg.ex.segment, data=seg.df, mean) 


##Plot hclust() results--------------------------------------------------------
plot(jitter(as.numeric(seg.df$gender)) ~ jitter(as.numeric(seg.df$subscribe)), 
     col=seg.ex.segment, yaxt="n", xaxt="n", ylab="", xlab="") 
     #seperates clusters by gender and subscription status     
     #where the color indicates segment membership


axis(1, at = c(1, 2), labels = c("Subscribe: No", "Subscribe: Yes")) #x-axis
axis(2, at = c(1, 2), labels = levels(seg.df$gender))                #y-axis


##3 K-Means--------------------------------------------------------------------
'
 The basic idea of k-means clustering is to define clusters then minimize the 
 total intra-cluster variation (known as total within-cluster variation). The 
 standard algorithm is the Hartigan-Wong algorithm (1979), which defines the 
 total within-cluster variation as the sum of squared distances Euclidean 
 distances between items and the corresponding centroid
 
 The distance measures will show how similar two elements (x, y) are and it will
 highly influence the results of the clustering analysis. The classical methods
 for distance measures is Euclidean distance.
'
# convert factor variables to numeric (kmeans requires). OK b/c all are binary.
seg.df.num <- seg.df
seg.df.num$gender    <- ifelse(seg.df$gender == "Male", 0, 1)
seg.df.num$ownHome   <- ifelse(seg.df$ownHome == "ownNo", 0, 1)
seg.df.num$subscribe <- ifelse(seg.df$subscribe == "subNo", 0, 1)

summary(seg.df.num)

set.seed(96743)

seg.k <- kmeans(seg.df.num, centers=4,nstart=25)

seg.k$centers #inspect the high-level differences between groups(means by group)

#check the distribution of income according to each cluster
boxplot(seg.df.num$income ~ seg.k$cluster, ylab = "Income", xlab = "Cluster")

'
 We want to view the result so We can use fviz_cluster. It is function can 
 provide a nice graph of the clusters. Usually, we have more than two dimensions
 (variables) fviz_cluster will perform principal component analysis (PCA) and 
 plot the data points according to the first two principal components that 
 explain the majority of the variance.
'
#clustering algorithms and visualization
install.packages('factoextra') #may take a while
library(factoextra)

fviz_cluster(seg.k, data = seg.df.num)

'
 The groups seems to be differentiated on key variables. With this information,
 an analyst could look up the group membership with key variables from the 
 earlier boxplot then look at the difference between groups in the clustering 
 results. This could lead to a business strategy suggestion. In our case, the 
 group with the highest income may lead to a potential marketing campaign.
'
