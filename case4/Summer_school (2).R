######load libraries-----
#You have to install the packages first using: install.package("package name")

#rm(list=ls()) -> use this function to remove pre-loaded data

setwd("C:\\Users\\Georgi\\Desktop\\Data science\\R-working") #set initial working directory
library(tidyr)
library(foreign)
library(tibble) 
library(dplyr)

library(lubridate)
library(dplyr)
library(imputeTS)
library(stats)
library(psych)
library(dendextend)
library(nFactors)
library(DescTools)

library(sentimentr)
library(stringr)
library(SnowballC)
library(quanteda)
library(GPArotation)
library(ggplot2)
library(lmtest)
#Importing and data prep----------------------------------------------------
#This sets the working directory
setwd("C:\\Users\\Georgi\\Desktop\\Data science\\R-working\\summer_school")

#perform bulk import for covid nums first
aux = list.files(path="C:\\Users\\Georgi\\Desktop\\Data science\\R-working\\summer_school")

dd = lapply(aux,read.csv)

#write a loop to combine the different csv tables into one data frame called "new
for (i in 1:length(dd)) {
  
  new = bind_rows(dd,dd[i])
  
}
#split "new" in two parts -> a and b
new1 = new[1:7617,]

new2 = new[7618:nrow(new),]

#Check names
a = names(new1)

b = names(covid_a)

#arrange dataframes
covid_a = new1[2:9]

covid_b = new2[c(4,5,6,11:ncol(new2))]

print(names(covid_a))
print(names(covid_b))

covid_c = covid_b[c(6,7,1,2,3,5,8,9)]

print(names(covid_a))
print(names(covid_c))

#synch names
colnames(covid_c) = names(covid_a)

#Bind the two dataframes
covid = rbind(covid_a,covid_c)
View(covid)

#Try to fix dates here:
covid1 = covid %>% separate(Last.Update,c("Date","Time"),sep =" ")

covid1$Date = ymd_hms(covid1$Date,frac=TRUE)

covid2 = covid1 %>% separate(Date,c("Datenew","X"),sep="T")

covid2 = covid2[-c(3,4)]

covid2$Datenew2 = as.Date(covid2$Datenew,"%d/%m/%y")

ifelse(covid2$Datenew == "%d/%m/%y",covid2$Datenew2 = as.Date(covid2$Datenew,"%d/%m/%y"),covid2$Datenew2 = as.Date(covid2$Datenew,"%y/%m/%d"))

sapply(covid, class)
colSums(is.na(covid2))
#combine the different regions into one country
covid_int = covid2 %>%
  group_by(Country.Region,Datenew) %>%
   summarize(daily_cases = sum(Confirmed),daily_deaths = sum(Deaths),daily_recovered = sum(Recovered))

#Plot data
plot(covid_int$daily_cases,type="l")

data = covid_int[,2:3]

windows()
ggplot(data,aes(x=Last.Update, y=daily_cases))+
  geom_line(color="#69b3a2", size=2, alpha=1.5)

#save a backup workspace
save.image("summer.RData")

#With the load command you can upload all the data simultaneously from your workspace directory
#Fro example "C:\\Users\\Georgi\\Desktop\\Data science\\R-working" is my workspaces directory
load("summer.RData")
#Check missing values
colSums(is.na(covid_int))

which(is.na(covid_int$daily_cases)==T)

covid_int = covid_int[-which(is.na(covid_int$daily_cases)==T),]

colSums(is.na(covid_int))

#Visualize pairs
windows()
pairs(covid_int[3:5])

#import policies data-------------------------------------------------
setwd("C:\\Users\\Georgi\\Desktop\\Data science\\R-working")

policies = read.csv("policies.csv",na.strings = c(NA,""," ","  "),stringsAsFactors = F)

#check variable type
class(policies)
class(covid_int)

#check the structure of the datasets
str(policies)
str(covid_int)

#Fix dates for covid_int
covid_int$Last.Update = ymd_hms(covid_int$Last.Update)

#Count missing values again
colSums(is.na(covid_int))

#Remove missing and unstructured dates!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!WARNING NA REMOVAL
covid_int = covid_int[-which(is.na(covid_int$Last.Update)==T),]

#Export from R to csv 
write.csv(covid_int,"C:\\Users\\Georgi\\Desktop\\Data science\\R-working\\covid_integrated2.csv", row.names = FALSE)

#import again
setwd("C:\\Users\\Georgi\\Desktop\\Data science\\R-working")

#Start from here--------------------------
library(tidyr)
library(foreign)
library(tibble) 
library(dplyr)

library(lubridate)
library(dplyr)
library(imputeTS)
library(stats)
library(psych)
library(dendextend)
library(nFactors)
library(DescTools)

library(sentimentr)
library(stringr)
library(SnowballC)
library(quanteda)
library(GPArotation)
library(ggplot2)

newcovid = read.csv("covid_integrated2.csv",na.strings = c(NA,""," ","  "),stringsAsFactors = F)
policies = read.csv("policies.csv",na.strings = c(NA,""," ","  "),stringsAsFactors = F)

#save.image("summerschool.RData")

#Begin analysis with "newcovid" file--------------------------------------
#Get only the data for Australia in separate dataframes
australia = newcovid[which(newcovid$Country.Region=="Australia"),]

australia_p = policies[which(policies$CountryName=="Australia"),]

#check class
sapply(australia_p,class)
sapply(australia,class)

#synch dates
australia$Datenew = as.Date(australia$Datenew,format = "%m/%d/%y")

australia_p$Date = as.Date(as.character(australia_p$Date), "%y%m%d")

#create sequence of dates
australia_p$Date = seq(ymd("2020-01-01"), ymd("2020-07-18"), "days")

colnames(australia) = c("Country","Date","cases","deaths","recovered")

australia = australia[-c(1,3,4)]
#Add number of recovered people fo australia
australia = left_join(australia_p,australia,by="Date")

#check correlations matrix

c = cor(australia[c(31,32,37:ncol(australia))])

windows()
cor.plot(c)

#Prepare stringency table
policies$counter = ifelse(policies$ConfirmedCases>0 & policies$StringencyIndex>=10,1,0)

policies$counter[is.na(policies$counter)==T] = 0

stringency_count = policies %>% 
  group_by(CountryName) %>% 
    summarize(Counter=sum(counter)/200) #200 is the number of days from 2020-01-01 to 2020-07-18

#save workspace
#save.image("summerschool.RData")
#K-means on counter
#load("summerschool.RData") -> type this command in order to import the last data

#Some descriptive statistics of the Stringency index

max(policies$StringencyIndex,na.rm = T)

min(policies$StringencyIndex,na.rm = T)

mean(policies$StringencyIndex,na.rm = T)

#Clustering---------------------------------------------------------------------
#Check zeros
which(stringency_count$Counter==0) # there are five countries with a value of 0, because of anavailiable data

#remove countries with zeros
stringency_count = stringency_count[-which(stringency_count$Counter==0),]

#Check for missing or inf data
which(is.na(stringency_count$Counter)==T)
which(is.infinite(stringency_count$Counter)==T)
which(is.nan(stringency_count$Counter)==T)

sapply(stringency_count,class)

#Convert characters to factors
stringency_count$CountryName = as.factor(stringency_count$CountryName)

k_means = kmeans(stringency_count[,2],centers = 3, nstart = 30, iter.max = 1000) 

stringency_count$Cluster = k_means$cluster

#Show centroids
plot(k_means$centers)

#Show clusters -> red = 2, black = 1, green = 3
windows()
plot(stringency_count$Counter, stringency_count$CountryName, pch=19, col = k_means$cluster ,xlab = "Quickness of reaction",ylab="Country")

#Sample 3 countries from the three clusters -> Kosovo, Latvia and Belgium------------------
#KOSOVO
Kosovo = newcovid[which(newcovid$Country.Region=="Kosovo"),]

Kosovo_p = policies[which(policies$CountryName=="Kosovo"),]

#synch dates
Kosovo$Datenew = as.Date(Kosovo$Datenew,format = "%m/%d/%y")

Kosovo_p$Date = as.Date(as.character(Kosovo_p$Date), "%y%m%d")

#create sequence of dates
Kosovo_p$Date = seq(ymd("2020-01-01"), ymd("2020-07-18"), "days")

colnames(Kosovo) = c("Country","Date","cases","deaths","recovered")

Kosovo = Kosovo[-c(1,3,4)]
#Add number of recovered people fo australia
Kosovo = left_join(Kosovo_p,Kosovo,by="Date")

#LATVIA
Latvia = newcovid[which(newcovid$Country.Region=="Latvia"),]

Latvia_p = policies[which(policies$CountryName=="Latvia"),]

#synch dates
Latvia$Datenew = as.Date(Latvia$Datenew,format = "%m/%d/%y")

Latvia_p$Date = as.Date(as.character(Latvia_p$Date), "%y%m%d")

#create sequence of dates
Latvia_p$Date = seq(ymd("2020-01-01"), ymd("2020-07-18"), "days")

colnames(Latvia) = c("Country","Date","cases","deaths","recovered")

Latvia = Latvia[-c(1,3,4)]
#Add number of recovered people fo australia
Latvia = left_join(Latvia_p,Latvia,by="Date")

#BELGIUM
Belgium = newcovid[which(newcovid$Country.Region=="Belgium"),]

Belgium_p = policies[which(policies$CountryName=="Belgium"),]

#synch dates
Belgium$Datenew = as.Date(Belgium$Datenew,format = "%m/%d/%y")

Belgium_p$Date = as.Date(as.character(Belgium_p$Date), "%y%m%d")

#create sequence of dates
Belgium_p$Date = seq(ymd("2020-01-01"), ymd("2020-07-18"), "days")

colnames(Belgium) = c("Country","Date","cases","deaths","recovered")

Belgium = Belgium[-c(1,3,4)]
#Add number of recovered people fo australia
Belgium = left_join(Belgium_p,Belgium,by="Date")

#Prepare a joint dataframe and analyze daily new cases and daily change in cases--------------------------------
#Fit multiple visualizations
#Then fit ANOVA
joint_table = bind_rows(Kosovo,Latvia,Belgium)

#Turn NA to 0
joint_table$ConfirmedCases[is.na(joint_table$ConfirmedCases)==T] = 0
#loop for daily changes
for (i in 1:nrow(joint_table)){

joint_table$daily_change_multiplier[i] = (joint_table$ConfirmedCases[i]/joint_table$ConfirmedCases[i-1])
  
}

#daily increase
for (i in 1:nrow(joint_table)){
joint_table$daily_new_cases[i] = joint_table$ConfirmedCases[i]-joint_table$ConfirmedCases[i-1]

}
#daily deaths
for (i in 1:nrow(joint_table)){
  joint_table$daily_deaths[i] = joint_table$ConfirmedDeaths[i]-joint_table$ConfirmedDeaths[i-1]
  
}

#Fix mistakes
joint_table = joint_table[-c(200,264,401),]

#Percentage of population infected
Kosovo_pop = 1845000
Belgium_pop = 11460000
Latvia_pop = 1920000

#hamal code - to be rewritten later!!!!
joint_table$Country_pop[1:199] = Kosovo_pop
joint_table$Country_pop[200:399] = Latvia_pop 
joint_table$Country_pop[399:597] = Belgium_pop 

#Calculate cases per pop 

joint_table$cases_per_1000_pop = joint_table$ConfirmedCases/joint_table$Country_pop*1000 #

joint_table = joint_table[-49]
#Save and load need files
#write.csv(joint_table,"C:\\Users\\Georgi\\Desktop\\Data science\\R-working\\joint_table.csv", row.names = FALSE)
#save.image("summerschool.RData")
setwd("C:\\Users\\Georgi\\Desktop\\Data science\\R-working")
#load("summerschool.RData")

#Modelling->ANOVA and LR---------------------------------------------

#get a more consise dataset
joint_table2 = joint_table[c(1:3,31:33,37,39,41,44,45:48)]

#type a function to get rid of NaNs
sapply(joint_table2,class)

#this piece of code removes Nans and NAs
joint_table2$daily_change_multiplier[which(is.nan(joint_table2$daily_change_multiplier)==T)] = 0
joint_table2$ConfirmedDeaths[which(is.na(joint_table2$ConfirmedDeaths)==T)] = 0
joint_table2$daily_change_multiplier[which(is.na(joint_table2$daily_change_multiplier)==T)] = 0
joint_table2$daily_new_cases[which(is.na(joint_table2$daily_new_cases)==T)] = 0
joint_table2$daily_change_multiplier[which(is.infinite(joint_table2$daily_change_multiplier)==T)] = 0
joint_table2$daily_deaths[which(is.na(joint_table2$daily_deaths)==T)] = 0

joint_table2$StringencyIndex[which(is.na(joint_table2$StringencyIndex)==T)] = 0
joint_table2$GovernmentResponseIndex[which(is.na(joint_table2$GovernmentResponseIndex)==T)] = 0
joint_table2$GovernmentResponseIndex[which(is.na(joint_table2$GovernmentResponseIndex)==T)] = 0
joint_table2$ContainmentHealthIndex[which(is.na(joint_table2$ContainmentHealthIndex)==T)] = 0
joint_table2$EconomicSupportIndex[which(is.na(joint_table2$EconomicSupportIndex)==T)] = 0
joint_table2$recovered[which(is.na(joint_table2$recovered)==T)] = 0


#Plot correclation matrix
c = cor(joint_table2[,4:14])

windows()
cor.plot(c)

#PCA on indexes:
PC_a = prcomp(joint_table2[6:9])

names(PC_a)

summary(PC_a)

PC_a$rotation 
#switch sign
PC_a$x = PC_a$x

PC_a$rotation = PC_a$rotation

#check squared error
a = (PC_a$sdev)^2/sum(PC_a$sdev^2)

joint_table2$Policy_index = PC_a$x[,1]

#corr matrix with the new index
cc = cor(joint_table2[,10:15])

windows()
cor.plot(cc)

#Fit ANOVA
anova = aov(cases_per_1000_pop~Policy_index,data=joint_table2)
summary(anova)

anova2 = aov(daily_deaths~Policy_index,data=joint_table2)
summary(anova2)

anova3 = aov(daily_new_cases~Policy_index,data=joint_table2)
summary(anova3)

anova4 = aov(daily_deaths~Policy_index,data=joint_table2)
summary(anova4)

kosovo_table = joint_table2[which(joint_table2$CountryName=="Kosovo"),]
latvia_table = joint_table2[which(joint_table2$CountryName=="Latvia"),]
belgium_table = joint_table2[which(joint_table2$CountryName=="Belgium"),]

#Some visualizations
windows()
plot(belgium_table$daily_change_multiplier,type = "l",col="blue")
#cutoff after 63rd day -> remove the 63th observation
belgium_table = belgium_table[-61,]

#Some visualizations after the first case
windows()
plot(belgium_table$daily_change_multiplier[54:nrow(belgium_table)],type = "l",col="blue")

windows()
plot(belgium_table$daily_new_cases[54:nrow(belgium_table)],type = "l",col="blue",xlab = "Time",ylab = "Daily cases")

windows()
plot(belgium_table$daily_deaths[54:nrow(belgium_table)],type = "l",col="blue")

windows()
plot(belgium_table$cases_per_1000_pop[54:nrow(belgium_table)],type = "l",col="blue")

windows()
plot(kosovo_table$daily_new_cases[64:nrow(kosovo_table)],type = "l",col="blue",xlab = "Time",ylab = "Daily cases")

windows()
plot(latvia_table$daily_new_cases[54:nrow(latvia_table)],type = "l",col="blue",xlab = "Time",ylab = "Daily cases")

windows()
plot(kosovo_table$daily_change_multiplier[64:nrow(kosovo_table)],type = "l",col="blue",xlab = "Time",ylab = "Daily cases")

#multiple plots
Belgium
windows()
plot(belgium_table$daily_new_cases[54:nrow(belgium_table)],type = "l",col="blue",xlab = "Belgium",ylab = "Daily cases and policy inxdex")
par(new=TRUE)
plot(belgium_table$StringencyIndex[54:nrow(belgium_table)],type="l",col="red",xlab = "Belgium",ylab = "Daily cases and policy inxdex")
#Kosovo
windows()
plot(kosovo_table$daily_new_cases[64:nrow(kosovo_table)],type = "l",col="blue",xlab = "Kosovo",ylab = "Daily cases and policy inxdex")
par(new=TRUE)
plot(kosovo_table$StringencyIndex[64:nrow(kosovo_table)],type="l",col="red",xlab = "Kosovo",ylab = "Daily cases and policy inxdex")
#Latvia
windows()
plot(latvia_table$daily_new_cases[64:nrow(latvia_table)],type = "l",col="blue",xlab = "Time",ylab = "Daily cases and policy inxdex")
par(new=TRUE)
plot(latvia_table$StringencyIndex[64:nrow(latvia_table)],type="l",col="red",xlab = "Time",ylab = "Daily cases and policy inxdex")

windows()
plot(belgium_table$daily_change_multiplier[66:nrow(belgium_table)],type = "l",col="blue",xlab = "Belgium and Kosovo",ylab = "Daily increase function")
par(new=TRUE)
plot(kosovo_table$daily_change_multiplier[66:nrow(kosovo_table)],type = "l",col="orange",xlab = "Belgium and Kosovo",ylab = "Daily increase function")
#load data
#load("summerschool.RData")

#Regression:
#Create lagged values
belgium_table$daily_new_cases_l12 = dplyr::lag(belgium_table$daily_new_cases,n=15)

#fix NAs
belgium_table$daily_new_cases_l12[which(is.na(belgium_table$daily_new_cases_l12)==T)] = 0

ccc = cor(belgium_table[15:16])

#Show correlation
print(paste("The correlation between policy index and daily cases in Belgium is:",ccc[2]))


#Check resduals
windows()
hist(eq1$resid, main="Histogram of Residuals",ylab="Residuals") #regression coef are skewed

print(mean(eq1$resid))

#Q-Q Plot
windows()
qqnorm(eq1$resid)
qqline(eq1$resid)

#joint regression
joint_table2$daily_new_cases_l12 = dplyr::lag(joint_table2$daily_new_cases,n=15)
#Fix NA
joint_table2$daily_new_cases_l12[which(is.na(joint_table2$daily_new_cases_l12)==T)] = 0

#Kosovo linear model
kosovo_table$daily_new_cases_l12 = dplyr::lag(kosovo_table$daily_new_cases,n=15)

kosovo_table$daily_new_cases_l12[which(is.na(kosovo_table$daily_new_cases_l12)==T)] = 0


ccc2 = cor(kosovo_table[15:16])
#Print correlation for Kosovo
print(paste("The correlation between policy index and daily cases in Kosovo is:",ccc2[2]))

#save.image("last.RData")

#load("last.RData")
#Logarithmic regressions
belgium_table$log_daily_cases = log(belgium_table$daily_new_cases)

belgium_table$log_policy = log(belgium_table$Policy_index)

#Clean NaNs and Inf
belgium_table$log_daily_cases[which(is.infinite(belgium_table$log_daily_cases)==T)] = 0
belgium_table$log_policy[which(is.nan(belgium_table$log_policy)==T)] = 0

#Create lagged values for daily log

belgium_table$daily_new_cases_log12 = dplyr::lag(belgium_table$log_daily_cases,n=12)

#Same for Kosovo

#Logarithmic regressions
kosovo_table$log_daily_cases = log(kosovo_table$daily_new_cases)

kosovo_table$log_policy = log(kosovo_table$Policy_index)

#Clean NaNs and Inf
kosovo_table$log_daily_cases[which(is.infinite(kosovo_table$log_daily_cases)==T)] = 0
kosovo_table$log_policy[which(is.nan(kosovo_table$log_policy)==T)] = 0

#Create lagged values for daily log

kosovo_table$daily_new_cases_log12 = dplyr::lag(kosovo_table$log_daily_cases,n=12)

#Logged linear models
eq4 = lm(daily_new_cases_log12~log_policy,data=belgium_table)
summary(eq4)

windows()
plot(belgium_table$log_policy,belgium_table$log_daily_cases,col = "blue",lwd=2,xlab = "Policy index",ylab = "Daily cases")
abline(lm(belgium_table$log_daily_cases ~ belgium_table$log_policy))

eq5 = lm(daily_new_cases_log12~log_policy,data=kosovo_table)
summary(eq5)

#Policiy impact assesment
beta = exp(eq4$coefficients)
print(beta)

beta2 = exp(eq5$coefficients)
print(beta2)

windows()
plot(kosovo_table$log_policy,kosovo_table$log_daily_cases,col = "blue",lwd=2,xlab = "Policy index",ylab = "Daily cases")
abline(lm(kosovo_table$log_daily_cases ~ kosovo_table$log_policy))

#Check resduals for Belgium regr
windows()
hist(eq4$resid, main="Histogram of Residuals",ylab="Residuals") #regression coef are skewed

print(mean(eq4$resid))

#Q-Q Plot
windows()
qqnorm(eq4$resid)
qqline(eq4$resid)


#Granger causality test:
grangertest(daily_new_cases_log12~log_policy,data=belgium_table,order =3)

grangertest(log_policy~daily_new_cases_log12,data=belgium_table,order =3)


#save.image("last.RData")





























