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

#loading packages
library(tidyverse)
library(here)

#read the data set
myData <- read_csv(here("DATA", "exam_nontidy.txt"))

#exploring the data set
head(myData)

# we have to read in the data set sensitive for tabulator delimiter
myData <- read_delim(here("DATA", "exam_nontidy.txt"), delim = "\t")

head(myData) 
## this looks better, now we can start to explore the dataset

# exploring the data and have a look at all variables in rows; basic summary stats
skimr::skim(myData)
# we have 1214 rows; 31 columns

## Tidy 1: We observe some variables starting with numbers, we want to rename these by using the pipe-rename.---- 
###This command is a nice way to check every column names(variables)
colnames(myData)

myData <- myData %>% 
  rename(Dose_asa_81 = `81asa`,
         Dose_asa_325 = `325asa`,
         feature_type = `feature type`)


head(myData)
tail(myData)
# Tidy 2: We observed the variable "id" has the two parts, we checked the codebook ----
# Tidy 2: of the dataset, that the first integer 1-4 indicates site of the study
# Tidy 2: so we use separate() function to separate the id column
myData <- myData %>% 
  separate(col = id, 
           into = c("site", "id"), 
           sep = "_")
# look at all variables
glimpse(myData) 
# check the distinct values of a column
as.factor(myData$feature_type)
nrow(myData$feature_type) ## doesn't count the rows 

nrow(distinct(myData,id))
head(myData) 
nrow(myData)
View(myData)
## there are several variables with the same id
## distinct of age and gender gives the rows that are unique for the combination of age and gender
## seems that the id variable contains also several consultations as the id is duplicated
## seem the reason is the feature type column that should be  spread in 2 separate cols 

# spred the feature type col in 2 separate
## We are keeping the same name of the object "myData". 
myData <- myData %>% pivot_wider(names_from = `feature_type`, values_from = feature_value)
## now every id is appears only once
## but warning and the 2 last cols are now list cols because not uniquely identified
glimpse(myData)
myData %>% count(`feature_type`)
myData %>% count(`feature_type`, feature_value)

## Tidy 3:---- 

myData <- myData %>% pivot_wider(names_from = `feature_type`, values_from = `feature_value`) 
##just looking again the data now
head(myData)
tail(myData)
summary(myData)
###the column feature_type and feature_value are not found here
skimr::skim(myData)

glimpse(myData) 
colnames(myData) 

# GET AN OVERVIEW of missing values
naniar::gg_miss_var(myData) 
## it seems the bleed variable contains a lot of missing values

# check the number of missing values
myData$bleed %>% is.na() %>% 
  sum() /## 1158
  nrow(myData) ##1214
## 95% of the bleed variable is missing (unnecessary variable!)

myData$bleed %>% is.na() %>% 
  sum() 

# subset the dataset without bleed var
myData <- myData %>% subset (select = -bleed)
glimpse(myData)
## the bleed variable is not part of the dataframe anymore

# find out duplicate column?
# are the last 2 variables expressing the same?
myData %>% select(30:31)

## it does not seem so. The 2 variables are expressing different values


###Try to view sod and pep columns

view(myData$sod)
view(myData$pep) ###from visual seems these two columns sod and pep are the same??? is it from feature type and feature_value?

myData %>% distinct(sod, pep)


###Dita (dinastryp) will do tidying: the some columns can include values from various features/measurements --- I have to confirm the variables feature_type/feature type and feature_value first is it changed into sod and pep? (CONFIRMED)
###I conclude the cokumn with various features/measurements was only id from original dataset(exam_nontidy.txt)

###look again the data and checking if I could find unusual unique characters and/or numbers which may indicate some variables have different features/measurements
colnames(myData)
skimr::skim(myData)
view(myData$sod_type)
view(myData$status)
head(myData$recpanc)
tail(myData$recpanc)
view(myData$Dose_asa_81)
view(myData$Dose_asa_325)
view(myData$brush)
head(myData)
tail(myData)

#-------------------------------------------------------------------------------
#-------------------Day6 Tasks: Tidy, adjust, and explore ----------------------

#Remove unnecessary columns from your dataframe: acinar, train, amp, pdstent
#Use subset() fuction to delete column by name (SOLVED)

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
myData <- 
  myData %>% 
  mutate(Newpep = pep)
myData <-
  myData %>% 
  mutate(Newpep = if_else(Newpep == "0", "No", "Yes"))

#  A numeric column showing multiplication of age and risk for each person
myData$age<-as.numeric(myData$age)
myData <- myData %>% 
  mutate(AgemultiplybyRisk = age*risk )


#Set the order of columns as: id, site, age and other columns

myData <- myData %>% 
  select(id, everything())

#Arrange ID column of your dataset in order of increasing number or alphabetically

arrange(myData, id)

#Read and join the additional dataset to your main dataset.
antibodyData <- read_delim(here("DATA", "exam_joindata.txt"), delim = "\t")

View(antibodyData) # Need to look over and tidy this data

# need to seperate the columns "id" and "antibody"
antibodyData <- antibodyData %>% 
  separate(col = id, 
           into = c("site", "id"), 
           sep = "_")

View(antibodyData) 

#Connect above steps with pipe. If using this pipe, do not use to code above. 
Fulldataset <- myData %>% 
  full_join(antibodyData %>% 
              separate(col = id, 
                       into = c("site", "id"), 
                       sep = "_"), by = c("id", "site"))


#Explore your data.Explore and comment on the missing variables.
is.na(Fulldataset)
naniar::gg_miss_var((Fulldataset), facet = gender) # 400 patients are missing antibody-variable whilst around 600 patients are missing the bleeding-variable. There are more NAs in teh female-population. These high numbers of NAs can be problematic for further analyses? 
###I do not understand why here is talking about bleeding-variable (bleed) again? I followed the commands above and it was already deleted(subset) because we agreed 95% missing so may be not relevant variable? (line 106 until 109)

skimr::skim(Fulldataset)

#Group by gender
Fulldataset %>% 
  group_by(gender) %>% 
  count()
# There are 476 females and 126 males in the dataset, this could be an explanation for the big difference in NAs between the genders. 

#Stratify your data by a categorical column and report min, max, mean and sd of a numeric column.
#Stratify your data by a categorical column and report min, max, mean and sd of a numeric column for a defined set of observations - use pipe!
Fulldataset %>% 
  group_by(gender) %>% 
  summarise(min(antibody, na.rm = T),max(antibody, na.rm = T), mean(antibody, na.rm = T), sd(antibody, na.rm = T))

#Only for persons with recpanc == 0
Fulldataset %>% 
  filter(recpanc == 0)

#Only for persons recruited in site 3
Fulldataset %>% 
  filter(site == 3)

#Only for persons older than 45
Fulldataset %>% 
  filter(age > 45)

#Only for persons with risk higher than 2 and sod_type is type 2
Fulldataset %>% 
  filter(risk > 2 & sod_type ==2) # No patients falls in this category. 

#Use two categorical columns in your dataset to create a table (hint: ?count)
## we want to look at Gender and "sod_type". 
Fulldataset %>% 
  select(gender, sod_type) %>% 
  count(gender, sod_type)

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


