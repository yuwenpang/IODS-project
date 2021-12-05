# Excise 5
# Data wrangling for human data

# Author; Yuwen Pang
# Date: 16/11/2021

# 1 Load the library
library(stringr)
library(dplyr)

# Set the working directory
setwd("Z:/Course/Opendata science/IODS-project")

# 2 Read the “Human development” and “Gender inequality” datas into R.
# The data combines several indicators from most countries in the world
hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)
gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

# 3 Explore the datasets
# the structure and dimensions of the data
str(hd);str(gii)
dim(hd);dim(gii)
summary(hd);summary(gii)

# remove the commas from GNI and print out a numeric version of it
str_replace(human$GNI, pattern=",", replace ="") %>% as.numeric()

# 4 rename the variables
# "Country" = Country name
# Health and knowledge
# "GNI" = Gross National Income per capita
# 'MYE' = "Mean.Years.of.Education"
# "Life.Exp" = Life expectancy at birth
# "Edu.Exp" = Expected years of schooling 
colnames(hd) <- c('HDI.R','Country','HDI','Life.Exp','Edu.Exp','MYE','GNI','GNI.R')

# Empowerment
# "Mat.Mor" = Maternal mortality ratio
# "Ado.Birth" = Adolescent birth rate
# "Parli.F" = Percetange of female representatives in parliament
# "Edu2.F" = Proportion of females with at least secondary education
# "Edu2.M" = Proportion of males with at least secondary education
# "Labo.F" = Proportion of females in the labour force
# "Labo.M" = Proportion of males in the labour force
colnames(gii) <- c('GII.R','Country','GII','Mat.Mor','Ado.Birth','Parli.F',
                  'Edu2.F','Edu2.M','Labo.F','Labo.M')

# 5 create two new variables
# "Edu2.FM" = Edu2.F / Edu2.M
# "Labo.FM" = Labo2.F / Labo2.M
edu2.FM <- gii$Edu2.F/gii$Edu2.M   #the ratio of Female and Male populations with secondary education in each country
labo.FM <- gii$Labo.F/gii$Labo.M #the ratio of labour force participation of females and males in each country 
# 5 Mutate the “Gender inequality” data
gii_add <- mutate(gii, Edu2.FM = edu2.FM, Labo.FM = labo.FM)
str(gii_add)
names(gii_add)

# 6 Join together the two datasets 
# using the variable Country as the identifier
human <- inner_join(hd, gii_add, by = "Country", suffix = c('.hd','.gii'))
names(human)

# 6 The joined data should have 195 observations and 19 variables
dim(human) #195*19

# 6 Call the new joined data "human" 
# 6 and save it in your data folder
write.csv(human, 'data/human.csv')

# Exercise 5 starting from here
# Data wrangling (max 5 points)
# 1 Load the last week produced 'human.csv'
human <- read.csv('data/human.csv')
head(human,n=6)

# 1 Explore the structure and the dimensions of the data 
str(human);
dim(human) #195*19

# 1 and describe the dataset briefly
# the human dataset contains 195 observations and 19 variables
summary(human)
# Here is the lable for the column names
# "Country" = Country name
# Health and knowledge
# "GNI" = Gross National Income per capita
# 'MYE' = "Mean.Years.of.Education"
# "Life.Exp" = Life expectancy at birth
# "Edu.Exp" = Expected years of schooling 
# "Mat.Mor" = Maternal mortality ratio
# "Ado.Birth" = Adolescent birth rate
# "Parli.F" = Percetange of female representatives in parliament
# "Edu2.F" = Proportion of females with at least secondary education
# "Edu2.M" = Proportion of males with at least secondary education
# "Labo.F" = Proportion of females in the labour force
# "Labo.M" = Proportion of males in the labour force

# 1 transform the Gross National Income (GNI) variable to numeric
# Notes attached here,
# Since there are commas(,) in most character records, 
# we cannot directly use as.numberic to convert chr to int
library(stringr)
summary(str_detect(human$GNI, ",")) # to detect how many commas excites
# summary shows 189 records contain commas
#    Mode   FALSE    TRUE 
# logical       6     189 

# Using str_replace_all to remove ','
GNI <- str_replace_all(human$GNI, ",", "")
human$GNI <- as.numeric(GNI)

# Alternatively, directly use this code
# str_replace(human$GNI, pattern=",", replace ="") %>% as.numeric()

# 2 Exclude unneeded variables
# "Country", "Edu2.FM", "Labo.FM", "Edu.Exp", "Life.Exp", 
# "GNI", "Mat.Mor", "Ado.Birth", "Parli.F";
col <- c("Country", "Edu2.FM", "Labo.FM", "Edu.Exp", "Life.Exp",
         "GNI", "Mat.Mor", "Ado.Birth", "Parli.F")
human_ex <- human[,col]

# or 
# select the 'keep' columns by using select()
# human_ex <- select(human, one_of(keep))

dim(human_ex) # 195*9

# 3. Remove all rows with missing values
# filter out all rows with NA values
human_ex <- na.omit(human_ex)
dim(human_ex) # now 162 records remained, 33 rows have been removed

# 4. Remove the observations which relate to regions instead of countries
# the last 7 records refer to regions rather than countries
n_keep <- nrow(human_ex)-7
human_ex <- human_ex[c(1:n_keep),]
dim(human_ex) #155*9

# 5. Define the row names by the country names and remove the country coloum
rownames(human_ex) <- human_ex$Country
human_ex_2 <- human_ex[,-1]
dim(human_ex_2) #155*8
str(human_ex_2)

# 6. Write the new human.csv
write.csv(human_ex_2, 'data/human_new.csv')

