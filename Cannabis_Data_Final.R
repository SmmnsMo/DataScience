library(dplyr)
library(tidyr)
library(knitr)
library(ggplot2)
library(boot)

# Reading in the data
Cannabis <- read.csv("Cannabis.csv")
View(Cannabis)

# Creating a subset of data that will be used. The omitted data are those that are redundant or are of no use to us. 
Cannabis_clean <- subset(Cannabis, select = -c(1,3,7,9,10,12,16,23,27,28,31:34,36:44,52))
View(Cannabis_clean)
# When sorting the dataset by name, we can see that there are duplicate names with different terpene values.

# Showing the duplicate values

View(Cannabis_clean %>% 
  group_by(Name) %>% 
  summarise(THC = mean(THC)))
  
Unique_Cannabis <- Cannabis_clean %>%
  distinct(Name, .keep_all = TRUE)
View(Unique_Cannabis)

# Creating a table of the average THC and CBD levels
Avg_Total_THC <- Cannabis_clean %>% 
       group_by(Name) %>% 
       summarise(Total.THC = mean(Total.THC))

Avg_THC <- Cannabis_clean %>%
  group_by(Name) %>%
  summarise(THC = mean(THC))

Avg_Thca <- Cannabis_clean %>%
  group_by(Name) %>%
  summarise(Thca = mean(Thca))

Avg_CBD <- Cannabis_clean %>%
  group_by(Name) %>%
  summarise(Total.CBD = mean(Total.CBD))

Avg_Cannabis <- Unique_Cannabis %>%
  mutate(Total.CBD = Avg_CBD$Total.CBD) %>%
  mutate(Total.THC = Avg_Total_THC$Total.THC) %>%
  mutate(THC = Avg_THC$THC) %>%
  mutate(Thca = Avg_Thca$Thca)

View(Avg_Cannabis)

Avg_Cannabis_Short <- Avg_Cannabis %>%
  select(c("Name", "THC", "Thca", "Total.THC"))
View(Avg_Cannabis_Short)

# Exploring the relationship between the ratings and both the THC content and the CBD content of cannabis.

# Creating a subset of our data which eliminates duplicate values

THC_ratings <- Avg_Cannabis %>%
  select(c("Rating", "Total.THC", "Total.CBD", "Type"))

# Exploring the correlation between our variables

cor(THC_ratings$Rating,THC_ratings$Total.THC)
cor(THC_ratings$Rating,THC_ratings$Total.CBD)

# Using both THC and CBD for a linear regression 

THC_CBD_model <- lm(Rating ~ Total.THC + Total.CBD, data = THC_ratings)
summary(THC_CBD_model)
plot(THC_CBD_model)

# We can see that a linear regression is not the best model for predicting ratings based on THC and CBD content.

THC_model <- lm(Rating ~ Total.THC, data = THC_ratings)
summary(THC_model)

CBD_model <- lm(Rating ~ Total.CBD, data = THC_ratings)
summary(CBD_model)
# Creating a scatter-plot for THC and CBD
THC_Scatter <- ggplot(THC_ratings, aes(Total.THC,Rating)) +
  geom_jitter() +
  geom_smooth(method = "lm")

THC_Scatter

CBD_Scatter <- ggplot(THC_ratings, aes(Total.CBD, Rating)) +
  geom_jitter() +
  geom_smooth(method = "lm")

CBD_Scatter

# Looking at strain Type

# Creating a linear regression for strain type and regression

Type_model <- lm(Rating~ Type, Unique_Cannabis)
summary(Type_model)
# We observe no significance between the three types.

# Strain type visualization:
Strain_Spread <- ggplot(Unique_Cannabis, aes(Type, Rating, color = Type)) +
  geom_jitter()

Strain_Spread

dataMedian <- summarise(group_by(Unique_Cannabis, Type), med = median(Rating))

Strain_Box_Spread <- ggplot(Unique_Cannabis,aes(Type, Rating)) +
  geom_boxplot() +
  geom_text(data = dataMedian, aes(x = Type, y = med, label = med), size = 3, vjust = -2)

Strain_Box_Spread

# We can see the spread of our data: The median for hybrid and sativa strains are directly in the middle whereas the median for indica strains are more concentrated in the upper quantile. The spread for indicas is the widest between the three strains, and Sativas have the most narrow spread. 

# We will explore the relationship between ratings and the effects of cannabis by creating a subset from our master data frame

effect_ratings <- Unique_Cannabis %>%
  select(c("Name", "Rating",37:49)) # columns 37 through 49 are the various effects of cannabis
View(effect_ratings)

effects_model <- lm(Rating~aroused + creative + energetic + euphoric + euphoric + focused + giggly + happy + hungry + relaxed + sleepy + talkative + tingly + uplifted, data = effect_ratings)
summary(effects_model)

# The summary shows euphoria, feeling giggly and talkative are all significant at the 95% confidence interval, and feeling happy is significant at the 90% confidence interval. We also note that the estimates for these effects are all negative for the linear regression equation with these variables having the largest negative values. 

plot(effects_model)
# Upon plotting the residual values of the effects, we see that the residual values of the data are very spread out on both sides of the linear equation showing that the model is very inaccurate. 

# Comparing average rating of strains based on its effects
# Creating average values for flavors

# Creating a function to reference and find the mean rating of the strains based on effects if binary = 1
Average <- function(x){
  mean(Unique_Cannabis[x>0,"Rating"])
}

# Loops the function over all of the feeling columns
list1 <- list()
for(i in 37:49) {
  list1[i-36]  = Average(Unique_Cannabis[i])
}

print(list1)

# Creating a table from the list created
Average_Effects <- data.frame(matrix(unlist(list1), nrow = 13))

# Renaming the column 
colnames(Average_Effects)[1] <- "Average_Rating"

# Adding names to the columns 
Average_Effects <- Average_Effects %>%
  mutate("Name" = colnames(Unique_Cannabis[37:49]))

View(Average_Effects)

Average_Effects_Bar <- ggplot(Average_Effects, aes(x = reorder(Name, -Average_Rating), Average_Rating)) +
  geom_bar(stat = "identity") +
  coord_cartesian(ylim= c(4,5)) +
  geom_text(data = Average_Effects, aes(label = round(Average_Rating, digits = 2)), vjust = -1)

Average_Effects_Bar
# 3 of the significant variables (tingly, giggly and talkative) have an average rating that is on the lower end of the data. 

# We want to do the same for the inverse of average effects - what is the average rating of the strain if it does not cause those effects?

Inverse_Average <- function(x){
  mean(Unique_Cannabis[x<1,"Rating"])
}

list1.2 <- list()
for(i in 37:49) {
  list1.2[i-36]  = Inverse_Average(Unique_Cannabis[i])
}

print(list1.2)

# Creating a table from the list created
Inverse_Average_Effects <- data.frame(matrix(unlist(list1.2), nrow = 13))

# Renaming the column 
colnames(Inverse_Average_Effects)[1] <- "Average_Rating"

# Adding names to the columns 
Inverse_Average_Effects <- Inverse_Average_Effects %>%
  mutate("Name" = colnames(Unique_Cannabis[37:49]))

View(Inverse_Average_Effects)

Inverse_Average_Effects_Bar <- ggplot(Inverse_Average_Effects, aes(x = reorder(Name, -Average_Rating), Average_Rating)) +
  geom_bar(stat = "identity") +
  coord_cartesian(ylim= c(4,5)) +
  geom_text(data = Inverse_Average_Effects, aes(label = round(Average_Rating, 2)), vjust = -1)

Inverse_Average_Effects_Bar

# Most interestingly the strains that do not cause users to feel uplifted on average have a lower rating (4.31)

# Next we will explore whether the flavors of cannabis have any effect on the ratings
flavor_ratings <- Unique_Cannabis %>% 
  select(c("Name", "Rating", 50:85)) # columns 50 through 85 are the various flavors of cannabis 
View(flavor_ratings)

flavor_model <- lm(Rating ~ apple + berry + blueberry + cheese + chemical + citrus + diesel + earthy + flowery + fruit + grape + grapefruit + honey + lavender + lemon + mango + menthol + none + orange + pepper + pine + pineapple + plum + pungent + rose + sage + skunk + spicy.herbal + strawberry + sweet + tea + tree + tropical + vanilla + violet + woody, data = flavor_ratings)
summary(flavor_model)

# We see that the only outstanding variable is vanilla flavor at an alpha level of .1.

# Creating Average ratings for Flavors
list2 <- list()
for(i in 50:85) {
  list2[i-49]  = Average(Unique_Cannabis[i])
}

print(list2)

# Part 1 of Avg flavors
Average_Flavor1 <- data.frame(matrix(unlist(list2), nrow = 36))

colnames(Average_Flavor)[1] <- "Average_Rating"

Average_Flavor <- Average_Flavor %>%
  mutate("Name" = colnames(Unique_Cannabis[50:85]))

View(Average_Flavor)

Average_Flavor1 <- Average_Flavor[1:18,]
View(Average_Flavor1)

Average_Flavor_Bar1 <- ggplot(Average_Flavor1, aes(x = reorder(Name, -Average_Rating), Average_Rating)) +
  geom_bar(stat = "identity") +
  coord_cartesian(ylim = c(4,5)) +
  geom_text(data = Average_Flavor1, aes(label = round(Average_Rating, 2)), vjust = -1) +
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"))

Average_Flavor_Bar1

# The average rating for strains that have no flavor are the worst rated on average.

# Part 2 of Avg flavors

Average_Flavor2 <- Average_Flavor[19:36,]

Average_Flavor_Bar2 <- ggplot(Average_Flavor2, aes(x = reorder(Name, -Average_Rating), Average_Rating)) +
  geom_bar(stat = "identity") +
  coord_cartesian(ylim = c(4,5)) +
  geom_text(data = Average_Flavor2, aes(label = round(Average_Rating, 2)), vjust = -1) +
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"))

Average_Flavor_Bar2

# Next we want to explore the inverse of Average flavor ratings:
list2.1 <- list()
for(i in 50:85) {
  list2.1[i-49]  = Inverse_Average(Unique_Cannabis[i])
}

print(list2.1)

Inverse_Average_Flavor <- data.frame(matrix(unlist(list2.1), nrow = 36))

colnames(Inverse_Average_Flavor)[1] <- "Average_Rating"

Inverse_Average_Flavor <- Inverse_Average_Flavor %>%
  mutate("Name" = colnames(Unique_Cannabis[50:85]))

View(Inverse_Average_Flavor)

# Part 1 of Avg Flavors

Inverse_Average_Flavor1 <- Inverse_Average_Flavor[1:18,]

Inverse_Average_Flavor_Bar1 <- ggplot(Inverse_Average_Flavor1, aes(x = reorder(Name, -Average_Rating), Average_Rating)) +
  geom_bar(stat = "identity") +
  coord_cartesian(ylim = c(4,5)) +
  geom_text(data = Inverse_Average_Flavor1, aes(label = round(Average_Rating, 2)),size = 3, vjust = -1)+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"))

Inverse_Average_Flavor_Bar1

# Part 2 of Avg Flavors

Inverse_Average_Flavor2 <- Inverse_Average_Flavor[19:36,]

Inverse_Average_Flavor_Bar2 <- ggplot(Inverse_Average_Flavor2, aes(x = reorder(Name, -Average_Rating), Average_Rating)) +
  geom_bar(stat = "identity") +
  coord_cartesian(ylim = c(4,5)) +
  geom_text(data = Inverse_Average_Flavor2, aes(label = round(Average_Rating, 2)),size = 3, vjust = -1) +
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"))

Inverse_Average_Flavor_Bar2

# The inverse flavor bar chart shows a mostly consistent average rating across all flavors.

# Using cross-validation to find a more accurate model to predict ratings from THC content
set.seed(1)

THC_CV_model <- glm(Rating ~ Total.THC, data = THC_ratings)

MSE_LOOCV_THC = cv.glm(THC_ratings, THC_CV_model)$delta[1]

MSE_LOOCV_THC

# Using cross-validation to find the mean squared error of CBD against ratings
set.seed(1)

CBD_CV_model <- glm(Rating ~ Total.CBD, data = THC_ratings)

MSE_LOOCV_CBD = cv.glm(THC_ratings, CBD_CV_model)$delta[1]

MSE_LOOCV_CBD

# Using cross-validation to find the mean squared error of Type against ratings
set.seed(1)

Strain_Type_Model <- glm(Rating ~ Type, data = THC_ratings)

MSE_LOOCV_Type = cv.glm(THC_ratings, Strain_Type_Model)$delta[1]

MSE_LOOCV_Type

# Using cross-validation to find the mean squared error of a model using effects to predict ratings

set.seed(1)

Effect_model <- glm(Rating ~ aroused + creative + energetic + euphoric + euphoric + focused + giggly + happy + hungry + relaxed + sleepy + talkative + tingly + uplifted , data = effect_ratings)

MSE_LOOCV_Effects = cv.glm(effect_ratings, Effect_model)$delta[1]

MSE_LOOCV_Effects