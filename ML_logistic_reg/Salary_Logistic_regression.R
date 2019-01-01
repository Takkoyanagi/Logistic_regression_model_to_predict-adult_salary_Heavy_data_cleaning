# Logistic Regression Project
#### R for data science and machine learning- taught by Jose Portilla
### dataset obtained from UCI adult dataset
### determine whether a given class makes <= 50K or >50K a year
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(Amelia)
library(plotly)
library(janitor)
library(caTools)
# Load files
df <- read.csv('adult_sal.csv')

#Check the data
head(df)

#remove extra X column
df <- select(df, -X)

#check data, confirm X1 column is removed.
head(df)

#split train and test data
set.seed(101)
sample <- sample.split(df$income, 0.75)
train <- subset(df, sample == TRUE)
test <- subset(df, sample == FALSE)
# quick analysis of the data
str(train)
summary(train)


# EDA & DATA CLEANING
# looks like there are a lot of factor levels in occupation, country, 
# type_employer, marital
# group some of these factors to reduce levels

# first, convert income to binary
train$income <- ifelse(train$income == '<=50K',0,1)

# and convert necessart features to factors
train$type_employer <- as.factor(train$type_employer)
train$country <- as.factor(train$country)
train$marital <- as.factor(train$marital)
train$income <- sapply(train$income, factor) #for fun, convert it in a different way

#let's take a look at the different levels of factor levels in marital
table(train$marital)

# then plot to see if we can group some cat. together
ggplot(train, aes(marital, fill = income)) + geom_histogram(stat = 'count', position = 'dodge')

# let's combine the categories to either married or single

married <- function(mar){
  mar <- as.character(mar)
  if (mar == 'Married-AF-spouse'| mar == 'Married-civ-spouse'){
    return ('Married')
  }else{
    return ('Single')
  }
}

train$marital <- as.factor(sapply(train$marital, married))
test$marital <- as.factor(sapply(test$marital, married))
table(train$marital)
# great, let's keep going

table(train$type_employer)
ggplot(train, aes(type_employer, fill = income)) + geom_histogram(stat = 'count', position = 'dodge')

# Let's combine Local-gov with State-gov as LS-gov
gr_gov <- function(gov){
  gov <- as.character(gov)
  if (gov == 'Local-gov' | gov == 'State-gov') {
    return ('LS-gov')    
  }else if(gov == 'Never-worked'| gov =='Without-pay') {
    return ('unemp')
  }else{
    return (gov)
  }
}

train$type_employer <- as.factor(sapply(train$type_employer, gr_gov))
table(train$type_employer)
test$type_employer <- as.factor(sapply(test$type_employer, gr_gov))
# Next group country
# seperate by region
Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North_America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin_and_South_America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

group_country <- function(country){
  if (country %in% Asia){
    return ("Asia")
  }else if (country %in% North_America){
    return ("North_America")
  }else if(country %in% Europe){
    return ('Europe')
  }else if (country %in% Latin_and_South_America){
    return ('Latin_and_South_America')
  }else{
    return ('Other')
  }
}

train$country <- as.factor(sapply(train$country, group_country))
table(train$country)
test$country <- as.factor(sapply(test$country, group_country))

# Now let's group the Age
gr_age <- function(age){
  age <- as.numeric(age)
  if (age<22){
    return('young')
  }else if (age < 37){
    return('prime')
  }else if(age < 55){
    return('veteran')
  }else{
    return ('retired')
  }
}

train$age <- as.factor(sapply(train$age, gr_age))
test$age <- as.factor(sapply(test$age, gr_age))
# Let's check if the categories aligns with income
ggplot(train, aes(age, fill = income)) + geom_histogram(stat = 'count', position = 'dodge')

# How does fnlwgt look?
ggplot(train, aes(fnlwgt, fill = income)) + geom_histogram(position = 'dodge') 

#no real trend, create 3 categories (low, med, high)
# use the max(train$fnlwgt)/3 

wgt <- function(fnl){
  if (fnl < 494902){
    return('low')
  }else if (fnl < 989803){
    return('med')
  }else{
    return('high')
  }
}

train$fnlwgt <- as.factor(sapply(train$fnlwgt, wgt))
test$fnlwgt <- as.factor(sapply(test$fnlwgt, wgt))
# Reduce education categories
ggplot(train, aes(education, fill = income)) + geom_histogram(stat = 'count', position = 'dodge')

elementary <- c('10th', '11th', '12th','1st-4th','5th-6th','7th-8th', '9th','Preschool')
assoc <- c('Assoc-acdm','Assoc-voc')
doc <- c('Doctorate', 'Prof-school')

educ <- function(ed){
  ed<-as.character(ed)
  if (ed %in% elementary){
    return('elementary')
  }else if(ed %in% assoc){
    return('assoc')
  }else if(ed %in% doc){
    return ('doc')
  }else{
    return(ed)
  }
}

train$education <- as.factor(sapply(train$education, educ))
test$education <- as.factor(sapply(test$education, educ))
# education number
ggplot(train, aes(education_num, fill = income)) + geom_histogram(stat = 'count', position = 'dodge')

# lets create catogories <9, 9-10, 11-12, 13, 14, 15>
#educ_num <- function(ednum){
  #ednum <- as.numeric(ednum)
  #if (ednum < 9){
    #return('low')
  #}else if (ednum <= 10){
    #return('normal')
  #}else if(ednum <=12){
    #return('scholar')
  #}else if(ednum == 13){
    #return('scholarplus')
  #}else if(ednum ==14){
    #return('inflection_pt')
  #}else{
    #return('sweet_spot')
  #}
#}

#train$education_num <- as.factor(sapply(train$education_num, educ_num))
#test$education_num <- as.factor(sapply(test$education_num, educ_num))
# race
ggplot(train, aes(race, fill = income)) + geom_histogram(stat = 'count', position = 'dodge')

# combine Amer-indian-eskimo with other
othrc<- function(race){
  race<-as.character(race)
  if (race=='Amer-Indian-Eskimo'){
    return ("Other")  
  }else{
    return(race)
  }
}

train$race <- as.factor(sapply(train$race,othrc))
test$race <- as.factor(sapply(test$race, othrc))

#capital_gain
ggplotly(ggplot(train, aes(capital_gain, fill = income)) + geom_histogram(position = 'dodge', stat = 'count'))

# create 2 categories >8000 or < 8000
cgain <- function(gain){
  gain <- as.numeric(gain)
  if (gain<8000) {
    return('low')    
  }else{
    return('high')
  }
}

train$capital_gain <- as.factor(sapply(train$capital_gain, cgain))
test$capital_gain <- as.factor(sapply(test$capital_gain, cgain))

#capital_loss, no real trend. Let's drop this column
ggplotly(ggplot(train, aes(capital_loss, fill = income)) + geom_histogram(position = 'dodge'))

# hr_per_week, split into 2 groups, < 40 and >40 hrs
ggplotly(ggplot(train, aes(hr_per_week, fill = income)) + geom_histogram(position = 'dodge'))

whrs <- function(whr){
  whr <- as.numeric(whr)
  if(whr<40){
    return('less_40hrs')
  }else{
    return('more_40hrs')
  }
}

train$hr_per_week <- as.factor(sapply(train$hr_per_week,whrs))
test$hr_per_week <- as.factor(sapply(test$hr_per_week, whrs))

#MISSING DATA
# replace '?' with NA 
train[train == '?'] <- NA
test[test == '?'] <- NA
#lets check one
table(train$occupation)

#entries for '?' is gone, let's confirm NA values
table(is.na(train$occupation))

#Let's tabulate NA values
missmap(train, y.at = c(1), y.labels = c(''), col = c('yellow','black'))
missmap(test, y.at = c(1), y.labels = c(''), col = c('yellow','black'))


# Looks like we are missing data from occupation and type_employer

str(train)
str(test)

# A/B testing we will decided whether dropping rows with NA values are better or
# dropping the occupation and type_employer column
train_omit <- na.omit(train)
test_omit <- na.omit(test)

# check the training set for removal of NA values
missmap(train_omit, y.at = c(1), y.labels = c(''), col = c('yellow','black'))

model1 <- glm(income ~., binomial(logit), train_omit)
summary(model1)
new_step_model1 <- step(model1)
summary(new_step_model1)
test_omit$predicted_income <- predict(model1, test_omit, type='response')
table(test_omit$income, test_omit$predicted_income > 0.5)

#accuaracy
cat("accuracy:", (5343+1122)/(5343+425+786+1122))

#precision
print(paste0("precision: ", (1122)/(1122+425)))

#recall
(1122)/(1122+786)


# drop column 
train_drop <- select(train, -occupation, -type_employer)
names(train_drop)
test_drop <- select(test, -occupation, -type_employer)

model2 <- glm(income~., binomial(logit), train_drop)
test_drop$predicted <- predict(model2, test_drop, type = 'response')
table(test_drop$income, test_drop$predicted>0.5)

#accuaracy
(5674+1152)/(5674+506+808+1152)

#precision
(1152)/(1152+506)

#recall
(1152)/(1152+808)

#conclusion dropping NA gave better results

# let's see how well logistic regression compares with randomforest

library(randomForest)

rFmodel <- randomForest(income~., ntree = 30,train_omit)
test_omit$rFpredict <- predict(rFmodel, select(test_omit, -income))
table(test_omit$income, test_omit$rFpredict)
#accuracy
(5335+1171)/(5335+1171+433+737)

