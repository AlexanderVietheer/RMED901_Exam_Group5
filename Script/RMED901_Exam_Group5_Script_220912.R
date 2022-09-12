#DATE: 220912
#NAME: Exam day 5-6
#DESCRIPTION: RMED901 groupexam


# read in the packages
library(tidyverse)
library(here)

#read inn the dataset
myData <- read_csv(here("DATA", "exam_nontidy.txt"))

# exploring the data set
head(myData)

# we have to read inn the dataset sensitive for tabulator delimitor
myData <- read_delim(here("DATA", "exam_nontidy.txt"), delim = "\t")

head(myData) 
## this looks better

# exploring the data and have a look at all variables in rows; basic summary stats
skimr::skim(myData)
## we have 1214 rows; 31 columns

#Dita (dinastryp) will do tidying: the some columns can include values from various features/measurements



