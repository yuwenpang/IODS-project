# Excerice 6: Analysis of longitudinal data
# Author: Yuwen Pang
# Date: 23-11-2021

# Data source: BPRS and RATS from GitHub repository of MABS.

# Library


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

# Convert the categorical variables