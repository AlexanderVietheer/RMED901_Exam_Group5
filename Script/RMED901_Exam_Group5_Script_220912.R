#Date: 20220912
#Group: RMED901- Exam group 5
#Group members: Alexander Vietheer, Dinastry Pramadita Zakiudin, 
#               Marta Espevold Hjelmeland and Shanshan Xu
#Description: Group 5 exam R script for the course:RMED901 22H / Data science with R for medical researchers


#-------------------------------------------------------------------------------
#-------------------Day 5 tasks:Read and tidy the dataset ----------------------

#write all the commands and document!
#tips:
#some columns may need to be separated
#some columns can be duplicated
#some column names can contain spaces or start with numbers
#some columns can include values from various features/measurements

# loading packages
library(tidyverse)
library(here)

#read the data set
myData <- read_csv(here("DATA", "exam_nontidy.txt"))

# exploring the data set
head(myData)

# we have to read in the data set sensitive for tabulator delimiter
myData <- read_delim(here("DATA", "exam_nontidy.txt"), delim = "\t")

head(myData) 
## this looks better, now we can start to explore the dataset

# exploring the data and have a look at all variables in rows; basic summary stats
skimr::skim(myData)

# we have 1214 rows; 31 columns

## Tidy 1: We observe some variables starting with numbers, we want to rename these by using the pipe-rename. 
myData <- myData %>% 
  rename(Dose_asa_81 = `81asa`,
         Dose_asa_325 = `325asa`)

skimr::skim(myData)

# Tidy 2: We observed the variable "id" has the two parts, we checked the codebook 
# Tidy 2: of the dataset, that the first integer 1-4 indicates site of the study
# Tidy 2: so we use separate() function to separate the id column

myData <- myData %>% 
  separate(col = id, 
           into = c("site", "id"), 
           sep = "_")



# find out duplicate column?

# find out some columns can include values from various features/measurements?


#-------------------------------------------------------------------------------
#-------------------Day6 Tasks: Tidy, adjust, and explore ----------------------

#Remove unnecessary columns from your dataframe: acinar, train, amp, pdstent
#Use subset() fuction to delete column by name

df=subset(myData, select = -c(acinar, train, amp, pdstent))

#Make necessary changes in variable types
#Create a set of new columns:
#  A column showing whether age is higher than 35 or not: values High/Low
library(dplyr)
myData <- myData %>% 
  mutate(agegroup = case_when(age <= 35 ~ "Low",
                              age > 35 ~ "High"))

#  A numeric column showing risk as a percentage of highest possible risk (5.5)

myData$risk <- as.numeric(myData$risk)
myData <- myData %>% 
  mutate(riskpercentage = risk / 5.5)

#  A column showing pep as No/Yes
##??? Did not find the column pep?

#  A numeric column showing multiplication of age and risk for each person
myData$age<-as.numeric(myData$age)
myData <- myData %>% 
  mutate(AgemultiplybyRisk = age*risk )


#Set the order of columns as: id, site, age and other columns

myData <- myData %>% 
  select(id, everything())

#Arrange ID column of your dataset in order of increasing number or alphabetically

arrange(myData, id, disp)

#Read and join the additional dataset to your main dataset.

#Connect above steps with pipe.

#Explore your data.Explore and comment on the missing variables.

#Stratify your data by a categorical column and report min, max, mean and sd of a numeric column.

#Stratify your data by a categorical column and report min, max, mean and sd of a numeric column for a defined set of observations - use pipe!

#Only for persons with recpanc == 0

#Only for persons recruited in site 3

#Only for persons older than 45

#Only for persons with risk higher than 2 and sod_type is type 2

#Use two categorical columns in your dataset to create a table (hint: ?count)


#-------------------------------------------------------------------------------
#-------Day7 Tasks: Create plots that would help answer these questions --------

#1.Are there any correlated measurements?

#2.Does the age distribution depend on sod_type?

#3.Does the age distribution of the patients depend on their sex (gender)?

#4.Does the risk score change with age of the patients?

#5.Does the aspirin usage depend on the age?


#-------------------------------------------------------------------------------
#------Day8 Tasks: Analyse the dataset and answer the following questions-------

#1.Does the outcome depend on the site where the procedure was performed?

#2.Does the outcome depend on the gender of the patient?

#3.Does the outcome depend on whether there was a trainee present during the procedure?

#4.According to the data, was the indomethacin reducing the risk of pancreatitis?


