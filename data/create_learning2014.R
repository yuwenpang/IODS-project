# Author: Yuwen Pang
# Date: 04-11-2021
# Subjects: Data wrangling exercise

# Set the working directory
setwd("Z:/Course/Opendata science/IODS-project")

# Access the dplyr library
library(dplyr)
library(ggplot2)
# install.packages('GGally')
library(GGally)


# Read the full learning2014 data from
lrn14 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header=TRUE)

# Explore the structure and dimensions of the data
str(lrn14)
dim(lrn14) # 183*60

# Create an analysis dataset
# Build the array to select the relatvent collown for each variable
deep_s <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
surf_s <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
stra_s <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")
deep_col <- select(lrn14, one_of(deep_s))
surf_col <- select(lrn14, one_of(surf_s))
stra_col <- select(lrn14, one_of(stra_s))

# Scale all combination variables and add the variables to new datasets
lrn14$deep <- rowMeans(deep_col)
lrn14$surf <- rowMeans(surf_col)
lrn14$stra <- rowMeans(stra_col)

# Look at the dataset
str(lrn14)

# choose a handful of columns to keep
keep_cols <- c("gender","Age","Attitude", "deep", "stra", "surf", "Points")

# select the 'keep_columns' to create a new dataset
lrn14_2 <- select(lrn14, one_of(keep_cols))

# Exclude observations where the exam points variable is zero
lrn14_3 <- filter(lrn14_2, Points > 0 )
dim(lrn14_3) #166*7: the 166 observations and 7 variables

# Set the working directory again
setwd("Z:/Course/Opendata science/IODS-project")
# Save the analysis dataset to the ‘data’
write.csv (lrn14_3, 'data/learning2014.csv')
write.table(lrn14_3, 'data/learning2014.txt')

# Test for reading the exported data
data_csv <- read.csv('data/learning2014.csv')
data_txt <- read.table('data/learning2014.txt')
# str()
str(data_csv)
str(data_txt)
# head()
head(data_csv, n=6)
head(data_txt, n=6)

# Section 2
# Analysis exercise 
# that focuses on performing and interpreting regression analysis
# Write a continuous report with a clear structure
stu14 <- read.csv('http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/learning2014.txt')
# Explore the structure and the dimensions of the data
str(stu14) #the same data to lrn14_3 i produced 
dim(stu14) #166*7
# describe the dataset briefly
# the data includes gender, age, attitude, deep, stra, surf and points

# Linner regression
# Choose three variables as explanatory variables 
# and fit a regression model where exam points is the target
# 1 testing or exploring the data, by plotining and pairs
stu14$gender <- as.factor(stu14$gender)
# str(stu14)
pairs(stu14[-1],col=stu14$gender)
p <- ggpairs(stu14, mapping = aes(col = gender,alpha = 0.3), 
         lower = list(combo = wrap("facethist", bins = 20)))
# print p
p

# As the pair correlation showed,
# the points as respond, the attitude, stra and surf as the explantory variables
model <- lm(points ~ attitude + stra + surf , data = stu14)
# summary the model
summary(model) # Multiple R-squared:  0.2074
# here reported that the surf would not present a acceptable contribution to the regression,
# so this variable would be discard in next time

# Examine the model
par(mfrow = c(2,2))
plot(model, which = c(1,2,5))
# it reported several outlines, such as records 35,56,77,145
# they should be removed in next time

# rebuild the fitted model
stu14_2 <- stu14[-c(35,56,77,145),] #162*7
model2 <- lm(points ~ attitude + stra , data = stu14_2)
summary(model2) # Multiple R-squared:  0.2921
# the r2 has been improved a bit in model2, from 0.21 to 0.29

# the diagnostic plots for model 2
par(mfrow = c(2,2))
plot(model2, which = c(1,2,5))