# The Analysis exercise focuses on exploring your data and performing 
# and interpreting logistic regression analysis. For completing the Analysis exercises, 
# include all the codes, your interpretations and explanations in the RMarkdown file chapter3.Rmd.

# Author: Yuwen Pang
# Date: 09-11-2021
# Subjects: Logical regression

# Load the library and tha data
library(dplyr)
library(ggplot2)
library(GGally)

# the reference to the data
# https://archive.ics.uci.edu/ml/datasets/Student+Performance

# Set the working directory
setwd("Z:/Course/Opendata science/IODS-project")

### Prapering the data
# Read both student-mat.csv and student-por.csv into R (from the data folder)
stu.mat <- read.csv('data/student-mat.csv', sep = ";", header = TRUE) #395*33
stu.por <- read.csv('data/student-por.csv', sep = ";", header = TRUE) #649*33
# alternatively, we could also read the data from url, like the samples shown in datacamp
# use the function of paste()

# Look at the data
head(stu.mat, n=6)
head(stu.por, n=6)

### Section 1 exploring the data
# explore the structure and dimensions of the data. (1 point)
str(stu.mat) #the structure
str(stu.por) #the structure
dim(stu.mat) #the dimensions 395*33
dim(stu.por) #the dimensions 649*33
colnames(stu.mat) #the colnames
colnames(stu.por) #the colnames


# Join the two data sets using all other variables than "failures", "paid", "absences", "G1", "G2", "G3" as (student) identifiers. 
# Keep only the students present in both datasets
# Which columns vary in datasets
free_cols <- c("failures","paid","absences","G1","G2","G3")

# The rest of the columns are common identifiers used for joining the datasets
# ?setdiff(), to look at the function of setdiff
join_cols <- setdiff(colnames(stu.por),free_cols)

# Join two data by join_cols
join_stu <- inner_join(stu.mat, stu.por, by = join_cols, suffix = c('.mat','.por'))

# Explore the structure and dimensions of the joined data
str(join_stu)
dim(join_stu) #Yes, here only 370 students listed in the joined data.370*39
colnames(join_stu)

# To combine the 'duplicated' answers in the joined data
# a) copy the solution from the DataCamp exercise
# print out the column names of 'math_por'
colnames(join_stu)

# create a new data frame with only the joined columns
alc <- select(join_stu, one_of(join_cols))

# the columns in the datasets which were not used for joining the data
notjoined_columns <- colnames(stu.mat)[!colnames(stu.mat) %in% join_cols]

# print out the columns not used for joining
notjoined_columns

# for every column name not used for joining...
for(col_name in notjoined_columns) {
  # select two columns from 'math_por' with the same original name
  two_cols <- select(join_stu, starts_with(col_name))
  # select the first column vector of those two columns
  first_col <- select(two_cols, 1)[[1]]
  
  # if that first column vector is numeric...
  if(is.numeric(first_col)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[col_name] <- round(rowMeans(two_cols))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[col_name] <- first_col
  }
}

# glimpse at the new combined data
glimpse(alc)
dim(alc) #370*33

# b) write your own solution to achieve this task
names(join_stu)
# to combine these colunms
free_cols <- c("failures","paid","absences","G1","G2","G3")

# The joined data should now have 370 observations:Yes
dim(alc) #370*33

# create a new column 'alc_use' to the joined data
# by using mutate() function
colnames(alc)
alc <- mutate(alc, alc_use = (Dalc + Walc)/2)

# use 'alc_use' to create a new logical column 'high_use' 
# which is TRUE for students for which 'alc_use' is greater than 2
alc <- mutate(alc, high_use = alc_use > 2)
alc$high_use

# plot the high_use by sex
p <- ggplot(data = alc, aes(x=high_use)) +
  geom_bar() + facet_wrap("sex")

# Glimpse at the joined and modified data 
glimpse(alc)

# Output the data
# Save the joined and modified data set to the ‘data’ folder, 
# 1) using for example write.csv() 
write.csv(alc, 'data/joint_stu.csv')

# or 2) write.table() functions
write.table(alc, 'data/joint_stu.txt')


### Section 2 Analyzing the regression
#  to study the relationships between high/low alcohol consumption 
# and some of the other variables in the data

# read previous step exported dataset, alc
stu.csv <- read.csv('data/joint_stu.csv', head = TRUE)
# stu.txt <- read.table('data/joint_stu.txt', head = TRUE)

# choose 4 interesting variables in the data and for each of them, 
# present your personal hypothesis about their relationships with alcohol consumption
# explore the data, and look at the potential explanatory variables for 'hihg_use'
colnames(stu.csv)


# choose 4 interesting variables
# "sex":meal seems more likely high use
# "age":with age grow, teens more likely alcohol drunker
# "absences": higher "absences" chould be seen in high_use
# "failures": high_use result in higher failures

# Numerically and graphically explore the distributions of your chosen variables
# cross-tabulations, bar plots and box plots
# draw pairs to find the potential explanatory variables
p <- ggpairs(stu.csv[,c(2,3,4,7,13,28,29,31,36)], 
             lower = list(combo = wrap("facethist", bins = 20)))
p1 <- ggplot(data = stu.csv, aes(x = high_use)) + ylab("sex") +
  geom_bar() + facet_wrap("sex")
p2 <- ggplot(data = stu.csv, aes(x = high_use)) + ylab("age") +
  geom_boxplot() + facet_wrap("age")
p3 <- ggplot(data = stu.csv, aes(x = high_use)) + ylab("absences") +
  geom_bar() + facet_wrap("absences")
p4 <- ggplot(data = stu.csv, aes(x = high_use)) + ylab("failures") +
  geom_bar() + facet_wrap("failures")

# print plots
# p;
p1;p2;p3;p4


# find the model with glm()
m <- glm(high_use ~ failures + absences + sex + age, data = stu.csv, family = "binomial")
summary(m)

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -3.72535    1.74828  -2.131 0.033100 *  
#   failures     0.56451    0.21098   2.676 0.007459 ** 
#   absences     0.09019    0.02334   3.864 0.000111 ***
#   sexM         1.00045    0.24754   4.042 5.31e-05 ***
#   age          0.10850    0.10505   1.033 0.301694 
# The 'sex' would be omited.

# compute odds ratios (OR)
OR <- coef(m) %>% exp
# (Intercept)    failures    absences        sexM         age 
# 0.02410474  1.75858403  1.09438639  2.71951309  1.11460134 

# compute confidence intervals (CI)
CI <- confint(m) %>% exp
# 2.5 %    97.5 %
#   (Intercept) 0.0007493589 0.7203136
# failures    1.1692521450 2.6916472
# absences    1.0477584017 1.1483784
# sexM        1.6839636172 4.4530085
# age         0.9076445049 1.3713869

# print out the odds ratios with their confidence intervals
cbind(OR, CI)

# modified glm models
m2 <- glm(high_use ~ failures + absences + age, data = stu.csv, family = "binomial")
summary(m2)

anova(m, m2, test="LRT")
# Analysis of Deviance Table
# 
# Model 1: high_use ~ failures + absences + sex + age
# Model 2: high_use ~ failures + absences + age
# Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
# 1       365     405.92                          
# 2       366     422.96 -1  -17.036 3.667e-05 ***
# The likelihood ratio test is highly significant and 
# so that the variable sex should remain in the model.

# explore the predictive power of you model
# predict() the probability of high_use
prob <- predict(m, type = "response")

# add the predicted probabilities to 'stu.csv'
stu.csv <- mutate(stu.csv, probability = prob)

# use the probabilities to make a prediction of high_use
stu.csv <- mutate(stu.csv, prediction = probability > 0.5)

# see the last ten original classes, predicted probabilities, and class predictions
select(stu.csv, failures, absences, sex, age, high_use, probability, prediction) %>% tail(10)

# the 2x2 cross tabulation 
# tabulate the target variable versus the predictions
tab <- table(high_use = stu.csv$high_use, prediction = stu.csv$prediction)

tab %>% prop.table()
tab %>% prop.table() %>% addmargins()

# prediction
# high_use FALSE TRUE
# FALSE   250    9
# TRUE     76   35
# mean(stu.csv$prediction==stu.csv$high_use)
# [1] 0.7702703; the accuracy and the error rate: 0.23



### Bonus
# Perform 10-fold cross-validation on your model
# define a loss function (average prediction error)
loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

# compute the average number of wrong predictions in the (training) data
loss_func(class = stu.csv$high_use, prob = stu.csv$probability)
# 0.2297297

# K-fold cross-validation
library(boot)
cv <- cv.glm(data = stu.csv, cost = loss_func, glmfit = m, K = 10)
# average number of wrong predictions in the cross validation
cv$delta[1] #0.2351351
# the ten-fold error is 0.235, a bit larger than the average error rate, 0.229

### Super-Bonus
# compare the performance of different logistic regression models
# by performing cross-validation
# build the prediction for models 2 in which without 'sex'
m2 <- glm(high_use ~ failures + absences + age, data = stu.csv, family = "binomial")
summary(m2)

prob2 <- predict(m2, type = "response")

# add the predicted probabilities to 'stu.csv'
stu.csv <- mutate(stu.csv, probability2 = prob2)

# use the probabilities to make a prediction of high_use
stu.csv <- mutate(stu.csv, prediction2 = probability2 > 0.5)

# calculating the cv for model 2
cv2 <- cv.glm(data = stu.csv, cost = loss_func, glmfit = m2, K = 10)
cv2$delta[1] # 0.2891892

# here i could see the model 2 has a higher cv error rate:0.289 than that of model 2:0.235
# conclusion, the 'sex' varible should be included in the logistic regression
