# Analysis of Longitudinal Data
# Author: Yuwen Pang
# Date: 26-11-2021

# Data wrangling section

# Library
library(dplyr)
library(tidyr)
library(ggplot2)

# Load data
bprs <- read.csv("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", sep="", header=TRUE)
rats <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", sep="", header=TRUE)

# check their variable names, 
# 11 variables in bprs and 13 in rats
names(bprs);names(rats)

# view the data contents 
head(bprs, n=6)
head(rats, n=6)
summary(bprs)
summary(rats)

# view structures
str(bprs)
str(rats)

# 2 Convert the categorical variables
bprs$treatment <- as.factor(bprs$treatment)
bprs$subject <- as.factor(bprs$subject)
rats$ID <- as.factor(rats$ID)
rats$Group <- as.factor(rats$Group)


## 3 Convert to long form
#Add a week variable to BPRS 
bprsl <- bprs %>% gather(key = weeks, value = bprs, -treatment, -subject)
# Extract the week number
bprsl <-  bprsl %>% mutate(week = as.integer(substr(weeks,5,5)))
# Take a glimpse at the bprsl data
glimpse(bprsl)

# Add a Time variable to rats
ratsl <- rats %>%
  gather(key = time, value = weight, -ID, -Group) %>%
  mutate(Time = as.integer(substr(time,3,4))) 

# Glimpse the data
glimpse(ratsl)

## 4 Compare the dimension of original data and longitudinal version
# therefor understanding the crucial of convert data form!
dim(bprs) #40*11
dim(rats) #16*13
dim(bprsl) #360*6
dim(ratsl) #176*5

# Export the cleared longitudinal data for analysis section

# Set the working directory
setwd("Z:/Course/Opendata science/IODS-project")

write.csv(ratsl, 'data/ratsl.csv')
write.csv(bprsl, 'data/bprsl.csv')
