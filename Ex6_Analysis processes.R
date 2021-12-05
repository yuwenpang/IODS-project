# Exercise 6: Analysis of longitudinal data
# Author: Yuwen Pang
# Date: 23-11-2021

# R Markdown
library(dplyr)
library(tidyr)
library(ggplot2)

####### 11
# Set the working directory
setwd("Z:/Course/Opendata science/IODS-project")

# read the longitudinal and original datasets
rats <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", sep="", header=TRUE)
bprs <- read.csv("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", sep="", header=TRUE)

bprsl <- read.csv('data/bprsl.csv', sep = ",", header = TRUE, stringsAsFactors = TRUE) 
ratsl <- read.csv('data/ratsl.csv', sep = ",", header = TRUE, stringsAsFactors = TRUE)

bprsl <- bprsl[,-1]
ratsl <- ratsl[,-1]

# Dimension of two form of data
dim(rats) #16*13
dim(ratsl) #176*5
dim(bprs) #40*11
dim(bprsl) #360*5

####### 22
names(ratsl)
str(ratsl)
names(bprsl)
str(bprsl)

# Convert Group and ID as factor variables
ratsl$ID <- as.factor(ratsl$ID)
ratsl$Group <- as.factor(ratsl$Group)
ratsl$Time <- as.factor(ratsl$Time)
str(ratsl)
# [1] "ID"     "Group"  "time"   "weight" "Time"
bprsl$treatment <- as.factor(bprsl$treatment)
bprsl$subject <- as.factor(bprsl$subject)
bprsl$week <- as.factor(bprsl$week)



# Draw the plot of ratsl
# Plot1
ggplot(ratsl, aes(x = Time, y = weight, color = ID)) +
  geom_point(size=0.5) +
  geom_line(aes(group = ID)) +
  facet_grid(. ~ Group, labeller = label_both) +
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(min(ratsl$weight), max(ratsl$weight)))

# Plot2
# ggplot(ratsl, aes(x = Time, y = weight, color = as.factor(ID))) +
#   geom_point(size=0.5) +
#   facet_grid(. ~ as.factor(Group), labeller = label_both) +
#   theme(legend.position = "none") + 
#   scale_y_continuous(limits = c(min(ratsl$weight), max(ratsl$weight)))

# Based on previous plots, I firstly group ratsl by Time
# Standardise the variable weight by Time
ratsl.gb <- ratsl %>%
  group_by(Group, Time) %>%
  mutate(stdweight = (weight - mean(weight))/sd(weight)) %>%
  ungroup()
dim(ratsl) #176*5
dim(ratsl.gb) #176*6
names(ratsl)

# Plot again with the standardised weight
ggplot(ratsl.gb, aes(x = Time, y = stdweight, linetype = ID)) +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  geom_line(aes(group = ID)) +
  facet_grid(. ~ Group, labeller = label_both) +
  scale_y_continuous(name = "standardized weight") +
  theme_bw()

# Number/levels of Times
n <- ratsl$Time %>% unique() %>% length()

# Summary rats with mean and standard error of weight by Group and Time 
ratss.gb <- ratsl %>%
  group_by(Group, Time) %>%
  summarise(mean = mean(weight), se = sd(weight)/sqrt(n)) %>%
  ungroup()
ratss.gb %>% summary()
dim(ratss.gb) #33*4

# Glimpse the grouped ratss data
glimpse(ratss.gb)

# Plot the mean profiles
names(ratss.gb) #"Group" "Time"  "mean"  "se"

ggplot(ratss.gb, aes(x = Time, y = mean, color = Group)) +
  geom_point(size=0.8) +
  geom_line(aes(group = Group)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=0.3) +
  scale_y_continuous(name = "mean(weight) +/- se(weight)") +
  theme_bw()


## Box polts
# Create a summary data by treatment and subject with mean as the summary variable (ignoring baseline week 0).
ratss.gb2 <- ratsl %>%
  group_by(Group, ID) %>%
  summarise(mean=mean(weight)) %>%
  ungroup()
dim(ratss.gb2) #16*3
# Glimpse the data
glimpse(ratss.gb2)

# Draw a boxplot of the mean versus group
ggplot(ratss.gb2, aes(x = Group, y = mean)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape=23, size=4, fill = "white") +
  scale_y_continuous(name = "mean(weight)") +
  theme_bw()

# Remove the outliers
ratss.gb3 <- ratss.gb2 %>%
  filter(mean < 550)
dim(ratss.gb3) #15*3
dim(ratss.gb2) #16*3

# Draw a boxplot of the mean versus ID
# ggplot(ratss.gb2, aes(x = ID, y = mean)) +
#   geom_boxplot() +
#   stat_summary(fun = "mean", geom = "point", shape=23, size=4, fill = "white") +
#   scale_y_continuous(name = "mean(weight)") +
#   theme_bw()

# Draw the new boxplot of the mean versus group
ggplot(ratss.gb3, aes(x = Group, y = mean)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape=23, size=4, fill = "white") +
  scale_y_continuous(name = "mean(weight)") +
  theme_bw()




# Fit the linear model with the mean as the response 
fit1 <- lm(mean ~ Group, data = ratss.gb3)

# Compute the analysis of variance table for the fitted model with anova()
anova(fit1)

# Fit the linear model with the mean as the response
names(ratss.gb)
# "Group" "Time"  "ID"    "mean"  "se"  

fit2 <- lm(mean ~ Group + Time, data = ratss.gb)

# Compute the analysis of variance table for the fitted model with anova()
anova(fit2)

## 3 Linear Mixed Effects Models
bprsl$treatment <- as.factor(bprsl$treatment)
bprsl$subject <- as.factor(bprsl$subject)
bprsl$week <- as.factor(bprsl$week)




## 3 Linear Mixed Effects Models 
# Library
library(dplyr)
library(tidyr)
library(ggplot2)

# Load data
bprs <- read.csv("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", sep="", header=TRUE)
rats <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", sep="", header=TRUE)

# check their variable names, view the data contents and structures
names(bprs);names(rats)
head(bprs, n=6)
head(rats, n=6)
str(bprs)
str(rats)
summary(bprs)
summary(rats)
dim(bprs) #40*11
dim(rats) #16*13
dim(bprsl) #360*6
dim(ratsl) #176*5

# Convert the categorical variables
bprsl$treatment <- as.factor(bprsl$treatment)
bprsl$subject <- as.factor(bprsl$subject)


# Draw the plot
ggplot(bprsl, aes(x = week, y = bprs, linetype = subject)) +
  geom_line(aes(group = subject)) +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  scale_y_continuous(limits = c(min(bprsl$bprs), max(bprsl$bprs))) +
  theme_bw()


# Exploring how bprs varied with treatment and time
# Number of subject
n <- bprsl$subject %>% unique() %>% length()

# Summary data with mean and standard error of bprs by treatment and week 
bprss <- bprsl %>%
  group_by(treatment, week) %>%
  summarise( mean = mean(bprs), se = sd(bprs)/sqrt(n) ) %>%
  ungroup()

# Plot the mean profiles of subjects
ggplot(bprss, aes(x = week, y = mean, linetype = treatment, shape = treatment)) +
  geom_line(aes(group = treatment)) +
  scale_linetype_manual(values = c(1,2)) +
  geom_point(size=3) +
  scale_shape_manual(values = c(1,2)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, linetype="1"), width=0.3) +
  scale_y_continuous(name = "mean(bprs) +/- se(bprs)") +
  theme_bw()


# create a regression model bprsl_reg
names(bprsl)
bprsl_reg <- lm(bprs ~ week + treatment, data = bprsl)

# print out a summary of the model
summary(bprsl_reg)

# access library lme4
library(lme4)

# Create a random intercept model
bprsl_ref <- lmer(bprs ~ week + treatment  + (1 | subject), data = bprsl, REML = FALSE)

# Print the summary of the model
summary(bprsl_ref)

# create a random intercept and random slope model
bprsl_ref1 <- lmer(bprs ~ week + treatment + (week | subject), data = bprsl, REML = FALSE)

# print a summary of the model
summary(bprsl_ref1)

# perform an ANOVA test on the two models
anova(bprsl_ref, bprsl_ref1)

# draw the plot of bprsl
ggplot(bprsl, aes(x = week, y = bprs, color = treatment)) +
  geom_line(aes(group = subject)) +
  theme_bw()

# Create a vector of the fitted values
Fitted <- fitted(bprsl_ref1)

# Create a new column fitted to RATSL
bprsl <- bprsl %>%
  mutate(Fitted)

# draw the plot of RATSL


ggplot(bprsl, aes(x = week, y = Fitted, linetype = subject)) +
  geom_line(aes(group = subject)) +
  # scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  theme_bw()


ggplot(bprsl, aes(x = week, y = bprs, linetype = subject)) +
  geom_line(aes(group = subject)) +
  # scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  scale_y_continuous(limits = c(min(bprsl$bprs), max(bprsl$bprs))) +
  theme_bw()




# Standardise the variable bprs
bprsl.gb <- bprsl %>%
  group_by(week) %>%
  mutate(stdbprs = bprs) %>%
  ungroup()
dim(bprsl) #360*5
dim(bprsl.gb) #360*6

# Plot again with the standardised bprs
ggplot(bprsl.gb, aes(x = week, y = stdbprs, linetype = as.factor(subject))) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  scale_y_continuous(name = "standardized bprs")

# Number of subjects(per week), baseline (week 0) included
# levels(as.factor(bprsl$subject)
n <- bprsl$subject %>% unique() %>% length()

# Summary data with mean and standard error of bprs by treatment and week 
bprss.gb <- bprsl %>%
  group_by(treatment, week) %>%
  summarise( mean = bprs, se = bprs) %>%
  ungroup()

# Glimpse the data
glimpse(bprss.gb)

# Plot the mean profiles
ggplot(bprss.gb, aes(x = week, y = mean, linetype = as.factor(treatment), shape = as.factor(treatment))) +
  geom_line() +
  scale_linetype_manual(values = c(1,2)) +
  geom_point(size=3) +
  scale_shape_manual(values = c(1,2)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, linetype="1"), width=0.3) +
  theme(legend.position = c(0.8,0.8)) +
  scale_y_continuous(name = "mean(bprs) +/- se(bprs)")

# Create a summary data by treatment and subject with mean as the summary variable (ignoring baseline week 0).
bprs8s <- bprsl %>%
  filter(week > 0) %>%
  group_by(treatment, subject) %>%
  summarise(mean=mean(bprs)) %>%
  ungroup()

# Glimpse the data
glimpse(bprs8s)

# Draw a boxplot of the mean versus treatment
ggplot(bprs8s, aes(x = as.factor(treatment), y = mean)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape=23, size=4, fill = "white") +
  scale_y_continuous(name = "mean(bprs), weeks 1-8")

# Create a new data by filtering the outlier and adjust the ggplot code the draw the plot again with the new data
dim(bprs8s) #40*3
bprsl8sl <- bprs8s[bprs8s$mean<70,]
dim(bprsl8sl) #39*3

ggplot(bprsl8sl, aes(x = as.factor(treatment), y = mean)) +
  geom_boxplot(color = 'grey') +
  stat_summary(fun = "mean", geom = "point", shape=23, size=4, fill = "white") +
  scale_y_continuous(name = "mean(bprs), weeks 1-8")

# Perform a two-sample t-test
t.test(mean ~ treatment, data = bprsl8sl, var.equal = TRUE)

# Add the baseline from the original data as a new variable to the summary data
bprsl8sl.2 <- bprs8s %>%
  mutate(baseline = bprs$week0)

# Fit the linear model with the mean as the response 
fit <- lm(mean ~ baseline + treatment, data = bprsl8sl.2)
plot(fit)
# Compute the analysis of variance table for the fitted model with anova()
anova(fit)

# Add a Time variable to rats
ratsl <- rats %>%
  gather(key = time, value = weight, -ID, -Group) %>%
  mutate(Time = as.integer(substr(time,3,4))) 

# Glimpse the data
glimpse(ratsl)

# Check the dimensions of the data
dim(ratsl) #176*5

# Plot the ratsl data
ggplot(ratsl, aes(x = Time, y = weight, group = ID, color = ID)) +
  geom_line() +
  facet_grid(~ Group) +
  theme_bw()

# create a regression model rats_reg
rats_reg <- lm(weight ~ Time + Group, ratsl)

# print out a summary of the model
rats_reg %>% summary()

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 244.0689     5.7725  42.281  < 2e-16 ***
#   Time          0.5857     0.1331   4.402 1.88e-05 ***
#   Group2      220.9886     6.3402  34.855  < 2e-16 ***
#   Group3      262.0795     6.3402  41.336  < 2e-16 ***


# M1 The linear mixed model for considering the random effects
library(lme4)  # for linear mixed model

rats_lmx <- lmer(weight ~ Time + Group + (1 | ID), data = ratsl, REML = FALSE)
rats_lmx %>% summary()

#             Estimate Std. Error t value
# (Intercept) 244.06890   11.73107   20.80
# Time          0.58568    0.03158   18.54
# Group2      220.98864   20.23577   10.92
# Group3      262.07955   20.23577   12.95

# Analysis the random effects of different groups
sum = 244.06890 + 0.58568 + 220.98864 + 262.07955
G2.e <- 220.98864/sum #0.30
G3.e <- 262.07955/sum #0.36

# M2 create a random intercept and random slope model
rats_lmx2 <- lmer(weight ~ Time + Group + (Time | ID), data = ratsl, REML = FALSE)

# print a summary of the model
rats_lmx2 %>% summary()

# perform an ANOVA test on the two models
anova(rats_lmx, rats_lmx2)

# Based on the reports of AIC, the original linear mixed model better fitted than the second one
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
# rats_lmx     6 1333.2 1352.2 -660.58   1321.2                         
# rats_lmx2    8 1194.2 1219.6 -589.11   1178.2 142.94  2  < 2.2e-16 ***