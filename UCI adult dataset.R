#using UCI adult dataset to predict if people in the data set,
#belong in a certain class by salary


df.data <- read.csv('adult_sal.csv')
#print(head(df.data))

df.data <- select(df.data, -X)
#print(head(df.data))

#using table to check the type of employer column by removing # before the line of code below
#print(table(df.data$type_employer))
#we found out there are 1836 null values and "without pay" is 14 while "never worked" is 7




#combining never worked and without pay into one group
comment <- "for(x in 1:nrow(df.data)){
  if(df.data$type_employer[x] == 'Never-worked' ||
     df.data$type_employer[x] == 'Without-pay'){
    df.data$type_employer[x] <- 'Unemployed'
    }
  }
"

#----- OR

comment <- "unemp <- function(job){
  job <- as.character(job)
  for(x in 1:length(job)){
    if(job[x] == 'Never-worked' || job[x] == 'Without-pay'){
      job[x] <- 'Unemployed'
    }
  }
  return(job)
}

df.data$type_employer <- unemp(df.data$type_employer)
"

#----- OR

comment <- "unemp <- function(job){
  job <- as.character(job)
  if (job=='Never-worked' | job=='Without-pay'){
    return('Unemployed')
  }else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer,unemp)
"

#----- OR

df.data$type_employer <- ifelse(df.data$type_employer == 'Never-worked',
                                'Unemployed',
                                ifelse(df.data$type_employer == 'Without-pay',
                                       'Unemployed',
                                       df.data$type_employer
                                       )
                                )

#combining state and local gov jobs to SL-gov
df.data$type_employer <- ifelse(df.data$type_employer == 'State-gov',
                                'SL-gov',
                                ifelse(df.data$type_employer == 'Local-gov',
                                       'SL-gov',
                                       df.data$type_employer
                                       )
                                )

#combining self employed jobs into self-emp to SL-gov
df.data$type_employer <- ifelse(df.data$type_employer == 'Self-emp-inc',
                                'self-emp',
                                ifelse(df.data$type_employer == 'Self-emp-not-inc',
                                       'self-emp',
                                       df.data$type_employer
                                       )
                                )

# view changes by removing # in the line of code below
#print(table(df.data$type_employer))


#looking at the marital column by removing # in the line of code below
#print(table(df.data$marital))

#reducing the marital status to just maried, never maried and not maried

for(x in 1:nrow(df.data)){
  if(df.data$marital[x] == 'Divorced' |
     df.data$marital[x] == 'Separated' |
     df.data$marital[x] == 'Widowed')
    {
    df.data$marital[x] <- 'Not-married'
    }
  else if(df.data$marital[x] == 'Never-married')
    {
    df.data$marital[x] <- 'Never-married'
    }
  else{
      df.data$marital[x] <- 'Married'
    }
}


#checking the country column by removing # the the line of code below
#print(table(df.data$country))

#grouping the country into continents
Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

continent <- function(cntry){
  if(cntry %in% Asia){
    return('Asia')
  }else if(cntry %in% North.America){
    return('North.America')
  }else if(cntry %in% Europe){
    return('Europe')
  }else if(cntry %in% Latin.and.South.America){
    return('Latin-and-South-America')
  }else{
    return('Other')
  }
}


df.data$continent <- sapply(df.data$country,continent)


# -----DEALING WITH MISSING DATA
library(Amelia)

#replacing all the '?' with NA
df.data[df.data == '?'] <- NA

#checking for the missing data
missmap(df.data,y.at = c(1),y.labels = c(''),col = c('yellow','black'))

#removing the NA values since theres basically nothing we can do about it
df.data <- na.omit(df.data)
missmap(df.data,y.at = c(1),y.labels = c(''),col = c('red','black'))


library(ggplot2)
print(ggplot(df.data,aes(age,fill = income))+
        geom_histogram(binwidth = 1, color ='black') +
        theme_bw())

print(ggplot(df.data,aes(hr_per_week,fill = income))+
        geom_histogram(binwidth = 1, bins = 30, color ='black') +
        theme_bw())


#converting some columns to factor
df.data$type_employer <- factor(df.data$type_employer)
df.data$education <- factor(df.data$education)
df.data$marital <- factor(df.data$marital)
df.data$occupation <- factor(df.data$occupation)
df.data$relationship <- factor(df.data$relationship)
df.data$race <- factor(df.data$race)
df.data$sex <- factor(df.data$sex)
df.data$income <- factor(df.data$income)
df.data$country <- factor(df.data$country)

#droping the country column 
df.data <- df.data %>% select(-country)


#renaming the continent column to region
df.data <- df.data %>% rename(region = continent)
#converting region column to factor
df.data$region <- factor(df.data$region)

print(ggplot(df.data,aes(region))+
        geom_bar(aes(fill = income, color ='black')) +
        theme_bw())



#Spliting the data
library(caTools)

set.seed(101)
sample <- sample.split(df.data$income,SplitRatio = 0.7)

df.data_Train <- subset(df.data,sample == T)
df.data_Test <- subset(df.data,sample == F)






# -----TRAINING THE MODEL

model <- glm(income ~., family = binomial(logit), data = df.data_Train)
#print(summary(model))


#Using the step() function which uses AIC to iteratively try to remove predictor variables 
#from the model and attempt to delete variables that do not significantly add to the fit. 

new.model <- step(model)
#print(summary(new.model))



# ------PREDICTING FROM THE MODEL


# ----- Creating a confussion matrix
predicted <- predict(model,df.data_Test,type = 'response')
table(df.data_Test$income, predicted >0.5)


#---- Accuracy of model
cat('Accuracy = ', (6288+1346)/(6288+1346+906+508))

# ----- Recall of the model
cat(' Recall = ', (6288)/(6288+508))

# ----- Precision of the model
cat(' Precision = ', (6288)/(6288+906))

      