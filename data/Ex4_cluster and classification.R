# Exercise 4: clustering and classification
# Author; Yuwen Pang
# Date: 16/11/2021

# 1 Load the library
library(MASS)
# for darwing pairs
library(ggplot2)
library(GGally)
library(corrplot) # the visual exploratory tool on correlation matrix

library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

# 1 Setting the working directory
setwd("Z:/Course/Opendata science/IODS-project")

# 2 Load the Boston data from the MASS package
data("Boston") #506*14

# 2 Explore the structure and the dimensions of the data
str(Boston)
summary(Boston)
dim(Boston) #506*14

# 3 Show a graphical overview of the data
pairs(Boston)
p <- ggpairs(Boston,
             lower = list(combo = wrap("facethist", bins = 20)))
p

# 3 Describe and interpret the outputs
# commenting on the distributions of the variables 
# and the relationships between them
cor_matrix <- cor(Boston)
# print the correlation matrix
cor_matrix %>% summary(round(digits = 2))

# 3 visualize the correlation matrix
corrplot(cor_matrix, method="circle", type = "upper",
         cl.pos = "b", tl.pos = "d", tl.cex = 0.6)
# (1) rad had significant high positive correlation with rax, accounting for 0.91
# (2) tax exhibited high correlations with all other variables

# 4 Standardize the dataset and print out
Boston_stand <- scale(Boston)
summary(Boston_stand)

# 4 Create a categorical variable of the crime rate
Boston_stand <- as.data.frame(Boston_stand)
# create a quantile vector of crim
bins <- quantile(Boston_stand$crim)
# create a categorical variable 'crime'
crime <- cut(Boston_stand$crim, breaks = bins, include.lowest = TRUE,
             lables = c("low", "med_low", "med_high", "high"))
table(crime)

# remove original crim from the dataset
Boston_stand <- dplyr::select(Boston_stand, -crim)

# add the new categorical value to scaled data
Boston_stand <- data.frame(Boston_stand, crime)

# 4 Divide the dataset to train and test sets
# number of rows in the Boston dataset 
n <- nrow(Boston_stand) #506
# choose randomly 80% of the rows
ind <- sample(n,  size = n * 0.8)

# create train set
train <- Boston_stand[ind,]
# create test set 
test <- Boston_stand[-ind,]

## Linear Discriminant analysis
# 5 Fit the linear discriminant analysis on the train set
lda.fit <- lda(crime ~. , data = train)
# print the lda.fit object
lda.fit

# Draw the LDA (bi)plot
# the function for lda biplot arrows
lda.arrows <- function(x, myscale = 1, arrow_heads = 0.1, color = "red", tex = 0.75, choices = c(1,2)){
  heads <- coef(x)
  arrows(x0 = 0, y0 = 0, 
         x1 = myscale * heads[,choices[1]], 
         y1 = myscale * heads[,choices[2]], col=color, length = arrow_heads)
  text(myscale * heads[,choices], labels = row.names(heads), 
       cex = tex, col=color, pos=3)
}

# target classes as numeric
classes <- as.numeric(train$crime)

# plot the lda results
plot(lda.fit, dimen = 2, col = classes, pch = classes)
lda.arrows(lda.fit, myscale = 1)

# 6 Save the crime categories from the test set
correct_classes <- test$crime
# 6 and then remove the categorical crime variable from the test dataset.
test <- dplyr::select(test, -crime)

# predict the classes with the LDA model on the test data
lda.pred <- predict(lda.fit, newdata = test)
# Cross tabulate the results
table(correct = correct_classes, predicted = lda.pred$class)
print(mean(correct_classes==lda.pred$class)) # the accuracy:0.6764706
# Comment on the results
# (1) the overall accuracy;0.67%
# (2) 'high' category had the highest accuracy rate

# 7 Reload the Boston dataset and standardize the dataset
data(Boston)
scl_boston <- scale(Boston)
# 7 Calculate the distances between the observations
dis_bos <- dist(scl_boston)

# 7 to identify the optimal number of clusters
# by silhouette
fviz_nbclust(scl_boston, kmeans, method = "silhouette")   # k=2
# 7 k-means algorithm
kmeans_bos <- kmeans(scl_boston, centers = 2)
# 7 Visualize the clusters
fviz_cluster(kmeans_bos, data = scl_boston, ellipse.type = 'convex', main = "kmeans of Boston")
# 7 interpret the results
table(kmeans_bos$cluster)
# (1) 177 for cluster 1; 329 for cluster 2.


##### Bonus
# Perform k-means on the original Boston data 
# with some reasonable number of clusters

fviz_nbclust(scl_boston, kmeans, method = "gap_stat")   # k=4

# the identified optiaml k number is 4
kmeans_bosv2 <- kmeans(scl_boston, centers = 4)

# perform LDA using the clusters as target classes
clas <- as.factor(kmeans_bosv2$cluster)

# add the k-means cluster to scaled Boston datasets
reBoston <- data.frame(scl_boston, clas)
# explore the new data set
str(reBoston)

# fixed lda
fix.lda <- lda(clas ~. , data = reBoston[,-1])
# Visualize the new rda results with a biplot
plot(fix.lda, dimen = 2, col = as.numeric(clas), pch = as.numeric(clas))
lda.arrows(fix.lda, myscale = 1)

# Which variables are the most influential linear separators for the clusters? 
# the tax and black seem as the most significant variables.

##### No Super-Bonus