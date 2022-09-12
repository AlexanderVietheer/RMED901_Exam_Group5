#DATE: 20220912
#NAME: Exam day 5-6
#DESCRIPTION: RMED901 group5 exam R script
#Day 5 tasks:Read and tidy the dataset
#We divided the tasks by the problems that we observed in the dataset.

# read in the packages
library(tidyverse)
library(here)

#read in the data set
myData <- read_csv(here("DATA", "exam_nontidy.txt"))

# exploring the data set
head(myData)

# we have to read in the data set sensitive for tabulator delimitor
myData <- read_delim(here("DATA", "exam_nontidy.txt"), delim = "\t")

head(myData) 
## this looks better, now we can start to explore the dataset

# exploring the data and have a look at all variables in rows; basic summary stats
skimr::skim(myData)

## we have 1214 rows; 31 columns
View(myData)

## TIDY 1: We observe some variables starting with numbers, we want to rename these by using the pipe-rename. 
myData1 <- myData %>% 
  rename(Dose_asa_81 = `81asa`,
         Dose_asa_325 = `325asa`)

skimr::skim(myData1)

# Tidy 2: We observed the variable "id" has the two parts, the number 1-4 could be 
# Tidy 2: the group number, and the number with four digits could be the ID number,
# Tidy 2: so we use separate() function to separate the id column

myData %>% 
  separate(col = id, 
           into = c("group", "id"), 
           sep = "_")







