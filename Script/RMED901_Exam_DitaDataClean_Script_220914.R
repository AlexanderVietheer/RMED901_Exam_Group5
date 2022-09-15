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

## Tidy 1: We observe some variables starting with numbers,and the variable "feature type" has space between the variable name, so we rename these by using the pipe-rename.

myData <- myData %>% 
  rename(Dose_asa_81 = `81asa`,
         Dose_asa_325 = `325asa`,
         feature_type = `feature type`)

## Tidy 2: We observed the variable "id" has the two parts, we checked the codebook of the dataset, that the first integer 1-4 indicates site of the study so we use separate() function to separate the id column.
myData <- myData %>% 
  separate(col = id, 
           into = c("site", "id"), 
           sep = "_")

## Tidy 3: remove the duplicated row in the dataset using the distinct() function.
myData<- myData %>% 
  distinct()
# there are 10 rows with duplicates final n = 1204 

## Tidy 4: Then we pivot the column into sod and pep columns
myData$feature_type <- as.factor(myData$feature_type)
myData$feature_value <- as.numeric(myData$feature_value)
myData <- myData %>% 
  pivot_wider(names_from = `feature_type`, values_from = feature_value)

# n = 602 (32 variables) after pivoting

# just looking again the data now
head(myData)
tail(myData)
summary(myData)
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
## the bleed variable is not part of the data frame anymore

# look again the data and checking if we could find unusual unique characters and/or numbers which may indicate some variables have different features/measurements.
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
#Use subset() fuction to delete column by name

df=subset(myData, select = -c(acinar, train, amp, pdstent))
# df is the data frame excluded the cloumns:acinar, train, amp, pdstent from the day 6 task.

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
  mutate(Newpep = pep, Newpep = if_else(Newpep == "0", "No", "Yes"))


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

# need to separate the columns "id" and "antibody"
antibodyData <- antibodyData %>% 
  separate(col = id, 
           into = c("site", "id"), 
           sep = "_")

#Connect above steps with pipe. If using this pipe, do not use to code above. 
Fulldataset <- myData %>% 
  left_join(antibodyData, by = c("id","site"))

# The Fulldataset contains n = 602 observations and 37 variables. We will work on this Fulldataset.

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
Fulldataset %>% 
  group_by(gender) %>% 
  summarise(min(risk, na.rm = T),max(risk, na.rm = T), mean(risk, na.rm = T), sd(risk, na.rm = T))

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

library("ggplot2")
library("devtools")
library("patchwork")

# loading library
library(ggplot2)
install.packages("corrplot")
library(corrplot)


##1.Are there any correlated measurements?

# In our data set, only the age, antibody and risk are the numeric variables,so we can explore the correlation betwwen age, risk, antibody. So we will plot the correlations between age, risk and antibody
# We can check the correlation by plotting the scatter plot of two variables first:
scatterplot1 <- ggplot(data = Fulldataset) + geom_point(mapping = aes(x = age, y = risk))
scatterplot2 <- ggplot(data = Fulldataset) + geom_point(mapping = aes(x = age, y = antibody))
scatterplot3 <- ggplot(data = Fulldataset) + geom_point(mapping = aes(x = risk, y = antibody))

# From the scatter plots, did not see a very clear relationship between age, risk and antibody.

# Then we create datafram of these three columns and then plot a correlation matrix.
df1 <- data.frame(age,risk,antibody)
correlationmatrix <- corrplot(cor(df1,use="pairwise.complete.obs" ),
                              addCoef.col = "black",
                              number.digits =  3,
                              outline = "black")
# age and risk were weakly negatively correlated, antibody did not show statistically significant correlation with age or risk.


#"2.Does the age distribution depend on sod_type? 
#age is numeric and sod_type is categorical

ggplot(Fulldataset,
       aes(x = sod_type, y = age)) +
  geom_col(aes(sod_type))
# It looks like there is an higher number of older patients with Type2. 

#Might this only depend on the higher number of older patient? Creating a boxplot to get an impression of the median: 
ggplot(Fulldataset,
       aes(x = sod_type, y = age)) +
  geom_boxplot(aes(sod_type)) # In this dataset, the age tend to be older in Type1 than Type2 and 3. 

##3.Does the age distribution of the patients depend on their sex (gender)?

ggplot(Fulldataset,
       aes(x = as.factor(gender), y = age)) + 
  geom_col(aes(fill = age), position = position_dodge()) +
  facet_wrap(facets = vars(gender)) 

# The patients were older in the female group, and younger in the male group.


#4.Does the risk score change with age of the patients? Alex 
#Both variables are numerical
ggplot(Fulldataset,
       aes(x = age, y = risk)) +
  geom_point(aes(age)) +
  geom_smooth() # visualize the trend
# it does not look like that there is a significant trend between risk and age.  

#5.Does the aspirin usage depend on the age? Dita

skimr::skim(myData$Dose_asa_81)
skimr::skim(myData$Dose_asa_325)
skimr::skim(myData$age)

head(Fulldataset$Dose_asa_81)
head(Fulldataset$age)

glimpse(Fulldataset$age)

view(Fulldataset$age)

###Using Fulldatapset

library(tibble)
library(MASS) # for ordinal log regression

#checking if anyone taking both 81 and 325 mg aspirin (should be FALSE if nobody took both)
myFullDataPlotAsp <- Fulldataset %>% add_column(checkAspUse = ifelse(Fulldataset$Dose_asa_81==1&Fulldataset$Dose_asa_325==1,1,0))

1 %in% myFullDataPlotAsp$checkAspUse ###Return FALSE: Nobody taking both aspirin dose 81 and 325 mg

summary(myFullDataPlotAsp$checkAspUse) ###Nobody taking both aspirin dose 81 and 325 mg

summary(myFullDataPlotAsp) ###I am using new dataframe myDataPlotAsp now when I add new column to check usage of both aspirin doses because I do not want to change clean data myData, also for later modifications by myself

###Dropping missing data from myDataPlotAsp
myFullDataPlotAsp <- drop_na(myFullDataPlotAsp)

###Converting (but creating a new variables to be safe, not changing the original ones) to factor
myFullDataPlotAsp$Asa81Fac <- as.factor(myFullDataPlotAsp$Dose_asa_81)
myFullDataPlotAsp$Asa325Fac <- as.factor(myFullDataPlotAsp$Dose_asa_325)

### To remove E-10212 bla bla (scientific notations on decimals)
options(scipen=999)

###Setting models

modelAsa81 <- glm(Asa81Fac ~ age, family='binomial', data=myFullDataPlotAsp)
summary(modelAsa81)

modelAsa325 <- glm(Asa325Fac ~ age, family='binomial', data=myFullDataPlotAsp)
summary(modelAsa81)

###Standard boxplot aspirin dose 81 mg
boxplot(age ~ Asa81Fac, data=myFullDataPlotAsp, xlab='Aspirin 81 mg Yes(1) No(0)',
        ylab='Age',
        main='Boxplot aspirin 81 mg and age')

###Alternative plot with data points aspirin dose 81 mg
ggplot(myFullDataPlotAsp, aes(x=Asa81Fac, y=age)) +
  geom_boxplot(size=.75) +
  geom_jitter(alpha=.5) +
  xlab('Aspirin, none (0), 81 mg (1)') +
  ylab('Age')

###Standard boxplot aspirin dose 325 mg
boxplot(age ~ Asa325Fac, data=myFullDataPlotAsp, xlab='Aspirin 325 mg Yes(1) No(0)',
        ylab='Age',
        main='Boxplot aspirin 325 mg and age')

###Alternative plot with data points aspirin dose 325 mg
ggplot(myFullDataPlotAsp, aes(x=Asa325Fac, y=age)) +
  geom_boxplot(size=.75) +
  geom_jitter(alpha=.5) +
  xlab('Aspirin, none (0), 325 mg (1)') +
  ylab('Age')

###Since nobody taking both 81 AND 325 mg, you can actually combine these two variables into e.g. 0 = no Asp, 1 = Asp 81, 2 = Asp 325. However, to test if the association is significant/not, you'll need to use orginal logistic regression (since the dependent/outcome variable is not binary).

###Converting Aspirin to categorical with 3 categories
myFullDataPlotAsp <- myFullDataPlotAsp %>% 
  add_column(CheckCatAsp = 
               ifelse(myFullDataPlotAsp$Dose_asa_81==1,1,                                                                   ifelse(myFullDataPlotAsp$Dose_asa_325==1,2,0)))

myFullDataPlotAsp$CheckCatAsp <- as.factor(myFullDataPlotAsp$CheckCatAsp)

###Creating plot for ordinal aspirin
ggplot(myFullDataPlotAsp, aes(x=CheckCatAsp, y=age)) +
  geom_boxplot(size=.75) +
  geom_jitter(alpha=.5) +
  xlab('Aspirin, none (0), 81 mg (1), 325 mg (2)') +
  ylab('Age')

#-------------------------------------------------------------------------------
#------Day8 Tasks: Analyse the dataset and answer the following questions-------

#1.Does the outcome depend on the site where the procedure was performed?

#2.Does the outcome depend on the gender of the patient?

#3.Does the outcome depend on whether there was a trainee present during the procedure?

#4.According to the data, was the indomethacin reducing the risk of pancreatitis?


