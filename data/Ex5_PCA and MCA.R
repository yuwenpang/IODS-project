# Chapter 5: Dimensional reduction techniques
# Author: Yuwen Pang
# Date: 19/11/2021

# The analysis section of Ex5: reduce the dimensions
# The PCA and MCA carried out

# 1 Show a graphical overview of the data 
# and show summaries of the variables in the data
library(GGally)
library(corrplot)

# Set the working directory
setwd("Z:/Course/Opendata science/IODS-project")

# 1. summary of the new human dataset
human_n <- read.csv('data/human_new.csv')
# attach the row names
row.names(human_n) <- human_n[,1]
human_n <- human_n[,-1]
names(human_n)

head(human_n,n=6)
str(human_n)
dim(human_n)

# 2. visualization by ggally
ggpairs(human_n)
summary(cor(human_n))
cor(human_n) %>% corrplot

# 2&3 Perform principal component analysis (PCA)
# 1 PCA carried out with origianl human dataset
# perform principal component analysis (with the SVD method)
pca_human <- prcomp(human_n)
s1 <- summary(pca_human)

# draw a biplot of the principal component representation and the original variables
biplot(pca_human, choices = 1:2)

# 2 PCA implemented by scaled human data
human_st <- scale(human_n)
pca_human_st <- prcomp(human_st)
s2 <- summary(pca_human_st)
biplot(pca_human_st, choices = 1:2)

# rounded percentages of variance captured by each PC
pca_pr1 <- round(100*s1$importance[2,], digits = 1) 
pca_pr2 <- round(100*s2$importance[2,], digits = 1)

# create object pc_lab to be used as axis labels
pc_lab1 <- paste0(names(pca_pr1), " (", pca_pr1, "%)")
pc_lab2 <- paste0(names(pca_pr2), " (", pca_pr2, "%)")

# draw a biplot
biplot(pca_human, cex = c(0.8, 1), col = c("black", "red"), xlab = pc_lab1[1], ylab = pc_lab1[2])

biplot(pca_human_st, cex = c(0.8, 1), col = c("black", "blue"), xlab = pc_lab2[1], ylab = pc_lab2[2])

# 3 Perform Multiple Correspondence Analysis (MCA)
# load the library
library(FactoMineR)
library(ggplot2)
library(dplyr)
library(tidyr)

# explore the tea dataset
# look at structure and dimension, 36 variables!
data(tea)
names(tea)
head(tea, n=6)
dim(tea) #300*36
str(tea)

# modify the tea
# column names to keep in the dataset
keep_columns <- c("Tea", "How", "how", "sugar", "where")

# select the 'keep_columns' to create a new dataset
tea_time <- dplyr::select(tea, keep_columns) #300*6
# Or 
# tea_time <- tea[,c("Tea", "How", "how", "sugar", "where")]
colnames(tea_time)
head(tea_time, n=6)
summary(tea_time)
str(tea_time)
levels(tea_time$Tea)
# [1] "black"     "Earl Grey" "green" 

# implement the mac
tea.mca.1 <- MCA(tea_time, graph = FALSE, method = 'indicator')
summary(tea.mca.1, nbelements = 3)


# plot the mca results
plot.MCA(tea.mca)
plot(tea.mca, invisible=c("ind"), habillage = "quali", graph.type = "classic")
# mca implemented with printing graph
tea.mca.2 <- MCA(tea_time, graph = TRUE, method = 'indicator')
# From the plot, (1) i could see the unpacked and tea shop had high components
# (2) the variables: 'where' and 'how' presented high reprentation weighs.

# additional mca graphs
library(factoextra)
fviz_mca_biplot(tea.mca.1, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())
fviz_mca_var(tea.mca.1, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
fviz_mca_var(tea.mca.1, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())