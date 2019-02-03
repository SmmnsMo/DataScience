# Exercise 0: Install these packages if you don't have them already

install.packages(c("cluster", "rattle.data","NbClust"))
library(dplyr)

# Now load the data and look at the first few rows

data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function

wine_clean <- scale(wine[-1])

head(wine_clean)

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                #This is the centroid of the clusters
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(wine_clean)

# Exercise 2:
#   * How many clusters does this method suggest?
# From the eye test it appears that the appropriate number of clusters is 3. 

#   * Why does this method work? What's the intuition behind it?
#This method works because it shows the dendrogram represented by the number of clusters.
#The larger the y-axis, the larger the difference is between clusters. We see the sum of squares varies greatly between the first and second clusters and declines shortly after.

#   * Look at the code for wssplot() and figure out how it works
# The code for wssplot finds the distance between the data points within the cluster and plots the number of clusters accordingly.  

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(wine_clean, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?

# This method suggests 3 clusters in the bar graph. 

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(wine_clean, centers = 3, nstart = 25)
fit.km
# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

table(wine$Type,fit.km$cluster)

fit.km$size

# We find that our clustering is an appropriate model. It correctly categorizes 95% of Type 1 wine, 100% of Type 2 wine and 94% of Type 3 wine. There is a very slim margin of error with our clustering categorization.

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

library(cluster)
clusplot(wine, fit.km$cluster)

# Most of the data points are reasonably categorized in their clusters. 
# This is a good example of clustering.