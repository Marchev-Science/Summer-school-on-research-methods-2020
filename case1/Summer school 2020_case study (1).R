#Case study - Summer school in Research methods 2020
#https://www.kaggle.com/zaurbegiev/my-dataset/tasks

# Import data ----
rm(list=ls()) 
setwd("C:\\Users\\Mi66ence\\Desktop\\BIG DATA\\summer school\\Case study")
dd=read.csv("credit_train.csv",na.strings= c ("NA", "", " ", "  "))

# Look at the data first ----
ddcl=data.frame(nm=names(dd),cl=sapply(dd,class),nas=colSums(is.na(dd)))
dd$Customer.ID=as.character(dd$Customer.ID)
sum(duplicated(dd$Customer.ID))
unique(dd$Customer.ID)
sum(duplicated(dd$Loan.ID))
unique(dd$Loan.ID)

# Fix duplicats ----
dd=dd %>% distinct(Customer.ID, .keep_all = TRUE)
sum(duplicated(dd$Loan.ID))

# Check nas ----
ddcl=data.frame(nm=names(dd),cl=sapply(dd,class),nas=colSums(is.na(dd)))
dd$Customer.ID[which(is.na(dd$Annual.Income==T))][1:10]
dd$Customer.ID[which(is.na(dd$Credit.Score==T))][1:10]
unique(dd$Annual.Income)
unique(dd$Credit.Score)
unique(dd$Current.Loan.Amount)
dd1=dd[which(is.na(dd$Annual.Income==T)),]
dd2=dd[which(is.na(dd$Annual.Income==T)),]
dd3=dd[which(dd$Current.Loan.Amount=="99999999"),]

dd$Customer.ID[which(is.na(dd$Annual.Income==T))][1:10]
dd$Customer.ID[which(dd$Current.Loan.Amount=="99999999")][1:10]
dd=dd[-which(dd$Current.Loan.Amount=="99999999"),]
ddi=dd[which(dd$Years.in.current.job=="n/a"),]

#Handle missing values----
# Chech missing values in rows
dd$nas=rowSums(is.na(dd))
quantile(dd$nas,probs=seq(0,1,0.01))
dd=dd[-which(dd$nas>=2),] #klienti, koito ne sa otgovarqli gi 4istim

table(dd$Loan.Status)
mean(dd$Annual.Income,na.rm=T)

ddcl=data.frame(nm=names(dd),cl=sapply(dd,class),nas=colSums(is.na(dd)))

dd$Months.since.last.delinquent=ifelse(is.na(dd$Months.since.last.delinquent==T),0,dd$Months.since.last.delinquent)

unique(dd$Bankruptcies)
dd=dd[-which(is.na(dd$Bankruptcies==T)),]
unique(dd$Maximum.Open.Credit)
dd=dd[-which(is.na(dd$Maximum.Open.Credit==T)),]
sapply(dd,class)

dd=dd[,-20]
dd$Customer.ID[which(dd$Years.in.current.job=="n/a")][1:10]
dd=dd[-which(dd$Years.in.current.job=="n/a"),]
unique(dd$Years.in.current.job)
table(dd$Years.in.current.job)

# Fix the class of the variables ----
unique(dd$Term)
dd$Term=as.factor(dd$Term)
unique(dd$Loan.Status)
dd$Loan.Status=ifelse(dd$Loan.Status=="Charged Off",1,0)
dd$Loan.Status=as.factor(dd$Loan.Status)


dd$Years.in.current.job=ifelse(dd$Years.in.current.job=="< 1 year",0,ifelse(dd$Years.in.current.job=="1 year",1,
      ifelse(dd$Years.in.current.job=="2 years",2,ifelse(dd$Years.in.current.job=="3 years",3,
      ifelse(dd$Years.in.current.job=="4 years",4,ifelse(dd$Years.in.current.job=="5 years",5,
      ifelse(dd$Years.in.current.job=="6 years",6,ifelse(dd$Years.in.current.job=="7 years",7,
      ifelse(dd$Years.in.current.job=="8 years",8,ifelse(dd$Years.in.current.job=="9 years",9,
      10)))))))))) #box loan status

unique(dd$Home.Ownership)
unique(dd$Purpose)
unique(dd$Monthly.Debt) #box loan status
unique(dd$Months.since.last.delinquent) #box loan status
unique(dd$Number.of.Open.Accounts) #box loan status
unique(dd$Number.of.Credit.Problems) #box loan status
unique(dd$Current.Credit.Balance) #box loan status
unique(dd$Maximum.Open.Credit) #box loan status
unique(dd$Bankruptcies) #box loan status
unique(dd$Tax.Liens) #box loan status


# Look at numeric and integer variables----
windows()
boxplot(Annual.Income~Loan.Status,data=dd, col=c("blue","red"))
windows()
boxplot(Years.in.current.job~Loan.Status,data=dd, col=c("blue","red"))
windows()
boxplot(Monthly.Debt~Loan.Status,data=dd, col=c("blue","red"))
windows()
boxplot(Months.since.last.delinquent~Loan.Status,data=dd, col=c("blue","red"))
windows()
boxplot(Number.of.Open.Accounts~Loan.Status,data=dd, col=c("blue","red"))
windows()
boxplot(Number.of.Credit.Problems~Loan.Status,data=dd, col=c("blue","red"))
windows()
boxplot(Current.Credit.Balance~Loan.Status,data=dd, col=c("blue","red"))
windows()
boxplot(Maximum.Open.Credit~Loan.Status,data=dd, col=c("blue","red"))
windows()
boxplot(Bankruptcies~Loan.Status,data=dd, col=c("blue","red"))
windows()
boxplot(Tax.Liens~Loan.Status,data=dd, col=c("blue","red"))
#we see a lot of outlyers


# Visualise Annual.Income vs.Purpose
library(googleVis)
pp=dd %>%
  group_by(Purpose) %>%
  summarise(mean.Income=mean(Annual.Income))
# Make a bar chart
plot(gvisBarChart(pp,options=list(width=1000, height=600, gvis.editor="Edit me!")))
#http://127.0.0.1:31329/custom/googleVis/BarChartID2074420665a.html

# Visualise Term vs.Credit.Score
pp=dd %>%
  group_by(Term) %>%
  summarise(mean.Credit.Score=mean(Credit.Score))
# Make a bar chart
plot(gvisBarChart(pp,options=list(width=1000, height=300, gvis.editor="Edit me!")))
#http://127.0.0.1:31329/custom/googleVis/BarChartID2074176f37c5.html

# Remove outlyers and make factor variables with levels /dummy - binning/
summary(dd$Annual.Income)
quantile(dd$Annual.Income,seq(0,1,0.05))
quantile(dd$Annual.Income,seq(0.99,1,0.001))
mean(dd$Annual.Income)
windows()
hist(dd$Annual.Income)

dd=dd[-which(dd$Annual.Income>10000000),]

#Years.in.current.job
round(prop.table(table(dd$Years.in.current.job,dd$Loan.Status),margin=1),2) 
class(dd$Years.in.current.job)

#Monthly.Debt
summary(dd$Monthly.Debt)
quantile(dd$Monthly.Debt,seq(0,1,0.05))
quantile(dd$Monthly.Debt,seq(0.99,1,0.001))
mean(dd$Monthly.Debt)
dd=dd[-which(dd$Monthly.Debt>93000),]

#Months.since.last.delinquent
summary(dd$Months.since.last.delinquent) # binning
quantile(dd$Months.since.last.delinquent,seq(0,1,0.05))

dd$Months.since.last.delinquent=ifelse(dd$Months.since.last.delinquent<=6,1,ifelse(dd$Months.since.last.delinquent<=12,2,ifelse(dd$Months.since.last.delinquent<=36,3,ifelse(dd$Months.since.last.delinquent<=48,4,ifelse(dd$Months.since.last.delinquent<=60,5,6)))))
round(prop.table(table(dd$Months.since.last.delinquent,dd$Loan.Status),margin=1),2) 
dd$Months.since.last.delinquent=ifelse(dd$Months.since.last.delinquent==2,1,0) #<=12 months
dd$Months.since.last.delinquent=as.factor(dd$Months.since.last.delinquent)

#Number.of.Open.Accounts
summary(dd$Number.of.Open.Accounts)
quantile(dd$Number.of.Open.Accounts,seq(0,1,0.05))
quantile(dd$Number.of.Open.Accounts,seq(0.95,1,0.001))
quantile(dd$Number.of.Open.Accounts,seq(0.99,1,0.001))
dd=dd[-which(dd$Number.of.Open.Accounts>36),]
dd$Number.of.Open.Accounts=ifelse(dd$Number.of.Open.Accounts<=8,1,ifelse(dd$Number.of.Open.Accounts<=10,2,ifelse(dd$Number.of.Open.Accounts<=14,3,4)))
round(prop.table(table(dd$Number.of.Open.Accounts,dd$Loan.Status),margin=1),2) 

#Number.of.Credit.Problems
summary(dd$Number.of.Credit.Problems) #?
quantile(dd$Number.of.Credit.Problems,seq(0,1,0.05))
quantile(dd$Number.of.Credit.Problems,seq(0.95,1,0.001))
table(dd$Number.of.Credit.Problems)
round(prop.table(table(dd$Number.of.Credit.Problems,dd$Loan.Status),margin=1),2) 

dd=dd[-which(dd$Number.of.Credit.Problems>4),]
round(prop.table(table(dd$Number.of.Credit.Problems,dd$Loan.Status),margin=1),2) 
dd$Number.of.Credit.Problems=ifelse(dd$Number.of.Credit.Problems>=3,1,0) #  problems - factor
table(dd$Number.of.Credit.Problems)
round(prop.table(table(dd$Number.of.Credit.Problems,dd$Loan.Status),margin=1),2) 
dd$Number.of.Credit.Problems=as.factor(dd$Number.of.Credit.Problems)

#Current.Credit.Balance
summary(dd$Current.Credit.Balance) # binning
quantile(dd$Current.Credit.Balance,seq(0,1,0.05))
quantile(dd$Current.Credit.Balance,seq(0.95,1,0.01))
quantile(dd$Current.Credit.Balance,seq(0.99,1,0.001))

dd=dd[-which(dd$Current.Credit.Balance>1500000),]

#Maximum.Open.Credit
summary(dd$Maximum.Open.Credit)
quantile(dd$Maximum.Open.Credit,seq(0.99,1,0.001))
dd=dd[-which(dd$Maximum.Open.Credit>10000000),]

#Credit.Score
summary(dd$Credit.Score) # binning?
quantile(dd$Credit.Score,seq(0,1,0.05))
quantile(dd$Credit.Score,seq(0.95,1,0.01))
quantile(dd$Credit.Score,seq(0.90,1,0.001))
summary(dd$Credit.Score[which(dd$Loan.Status==1)]) 

#Bankruptcies
summary(dd$Bankruptcies)
quantile(dd$Bankruptcies,seq(0,1,0.05))
quantile(dd$Bankruptcies,seq(0.99,1,0.001))
summary(dd$Bankruptcies[which(dd$Loan.Status==1)]) 
dd=dd[-which(dd$Bankruptcies>1),]
table(dd$Bankruptcies)
round(prop.table(table(dd$Bankruptcies,dd$Loan.Status),margin=1),2) 

#Tax.Liens
summary(dd$Tax.Liens)
quantile(dd$Tax.Liens,seq(0,1,0.05))
quantile(dd$Tax.Liens,seq(0.99,1,0.001))

round(prop.table(table(dd$Tax.Liens,dd$Loan.Status),margin=1),2) 
table(dd$Tax.Liens[which(dd$Loan.Status==1)]) 
dd$Tax.Liens=ifelse(dd$Tax.Liens>=2,1,0)
dd$Tax.Liens=as.factor(dd$Tax.Liens)

# Look at factor variables
library(ggplot2)
windows()
ggplot(data=dd)+geom_bar(aes(x=Term,fill=(Loan.Status))) #Term - factor
round(prop.table(table(dd$Term,dd$Loan.Status),margin=1),2) 
windows()
ggplot(data=dd)+geom_bar(aes(x=Home.Ownership,fill=(Loan.Status)))
round(prop.table(table(dd$Home.Ownership,dd$Loan.Status),margin=1),2) 
dd$Home.Ownership=ifelse(dd$Home.Ownership=="HaveMortgage",0,1)
dd$Home.Ownership=as.factor(dd$Home.Ownership)
round(prop.table(table(dd$Home.Ownership,dd$Loan.Status),margin=1),2) 

windows()
ggplot(data=dd)+geom_bar(aes(x=Purpose,fill=(Loan.Status)))
round(prop.table(table(dd$Purpose,dd$Loan.Status),margin=1),2) 

dd$Purpose=ifelse(dd$Purpose=="small_business"|dd$Purpose=="moving"|dd$Purpose=="Business Loan",1,0)
dd$Purpose=as.factor(dd$Purpose)
round(prop.table(table(dd$Purpose,dd$Loan.Status),margin=1),2) 
dd=dd[,-(1:2)]
sapply(dd,class)

windows()
ggplot(data=dd)+geom_bar(aes(x=Months.since.last.delinquent,fill=(Loan.Status)))
round(prop.table(table(dd$Months.since.last.delinquent,dd$Loan.Status),margin=1),2) 

dd$Term=ifelse(dd$Term=="Short Term",1,0)
dd$Term=as.factor(dd$Term)

# Split data into training and validation set
set.seed(1)
train=sample(nrow(dd),round(nrow(dd)*3/4))
test=sample(nrow(dd),round(nrow(dd)*1/4))
table(dd$Loan.Status)
table(dd$Loan.Status[train])
table(dd$Loan.Status[test])
16022/nrow(dd) #0.30 baseline probability

# Make a classification tree----
library(tree)
eq1=tree(Loan.Status~.,data=dd, subset=train)
# Visualize the tree
windows()
plot(eq1)
text(eq1)
eq1

#Feature engineering----
dd$perc=dd$Monthly.Debt/(dd$Annual.Income/12)

# Corelation matrix ----
sapply(dd,class)

dd$LoanStatusnumeric=dd$Loan.Status
dd$LoanStatusnumeric=as.numeric(dd$LoanStatusnumeric)
dd_n=select_if(dd,is.numeric)

library(psych)
windows()
cor.plot(cor(dd_n),numbers = T, las=2)

dd=dd[,-19]



# Fit several competing regression equations----
# Estimate a logistic regression equation----
eq2=glm(Loan.Status~.,data=dd,subset=train, family=binomial)
summary(eq2)
class(dd$Loan.Status)
unique(dd$Loan.Status)

eq3=glm(Loan.Status~Current.Loan.Amount+Term+Credit.Score+Annual.Income+Purpose+Monthly.Debt+Years.of.Credit.History+Months.since.last.delinquent+Number.of.Open.Accounts+Current.Credit.Balance+Maximum.Open.Credit+perc,data=dd,subset=train, family=binomial)
summary(eq3)

eq3=glm(Loan.Status~Term+Credit.Score+perc+Current.Loan.Amount+Annual.Income+Monthly.Debt+Current.Credit.Balance+Purpose+Number.of.Open.Accounts+Years.of.Credit.History+Months.since.last.delinquent,data=dd,subset=train, family=binomial)
summary(eq3)

# Fit LDA $ QDA models ----
library(MASS)
eq4=lda(Loan.Status~., data=dd,subset=train)
eq4
eq4=lda(Loan.Status~Current.Loan.Amount+Term+Credit.Score+Annual.Income+Purpose+Months.since.last.delinquent+Maximum.Open.Credit+perc, data=dd,subset=train)
eq4
eq5=qda(Loan.Status~., data=dd,subset=train)
eq5 
eq5=qda(Loan.Status~Current.Loan.Amount+Term+Credit.Score+Annual.Income+Purpose+Maximum.Open.Credit+perc, data=dd,subset=train)
eq5 

#QDA with LAsso variables
eq7=qda(Loan.Status~Term+Credit.Score+perc+Current.Loan.Amount+Maximum.Open.Credit+Annual.Income+Purpose, data=dd,subset=train)
eq7 
#Current.Loan.Amount,Credit.Score,Term,Maximum.Open.Credit,Annual.Income,Current.Credit.Balance,Purpose3,Home.Ownership3,Number.of.Open.Accounts3,Years.of.Credit.History,Purpose2,Monthly.Debt,Home.Ownership2,Months.since.last.delinquent1,Tax.Liens


# Best Subset selection ----
library(leaps)
# Best subset selection
aux=regsubsets(Loan.Status~Current.Loan.Amount+Term+Credit.Score+Annual.Income+Purpose+Monthly.Debt+Years.of.Credit.History+Months.since.last.delinquent+Number.of.Open.Accounts+Current.Credit.Balance+Maximum.Open.Credit+perc,data=dd[train,],nvmax=12, method="exhaustive")
auxs=summary(aux)
auxs
names(auxs)
auxs$cp 
#Credit.Score,Term, perc,Annual.Income,Monthly.Debt,Maximum.Open.Credit,Purpose1,Current.Loan.Amount,
#Current.Credit.Balance, Years.of.Credit.History,Number.of.Open.Accounts,Months.since.last.delinquent1,


#LOGISTIC REGRESSION WITH lASSO
eq20=glm(Loan.Status~Current.Loan.Amount+Term+Credit.Score+Annual.Income+Current.Credit.Balance+Purpose+Years.of.Credit.History+Number.of.Open.Accounts+Monthly.Debt+Months.since.last.delinquent+perc,data=dd,subset=train, family=binomial)
summary(eq20)

#Fit Lasso ----
library(glmnet)
dds=dd

#dds=dds[,-20]
x=model.matrix(Loan.Status~.,dds)
x=x[,-1]
x=scale(x,center=T,scale=T)
y=dds$Loan.Status

cv=cv.glmnet(x[train,],y[train],alpha=1,standartize=F,family="binomial") 
windows()
plot(cv)
cv$lambda.min

# Fit lasso model 
eq23=glmnet(x[train,],y[train],alpha = 1, lambda = cv$lambda.min,standardize = F,family="binomial")
eq23$beta
aux.lasso=data.frame(feature=rownames(eq23$beta),abs.value=abs(as.numeric(eq23$beta)))
aux.lasso=aux.lasso %>%
  arrange(desc(abs.value))
View(aux.lasso) # vivdame koi sa naj-vajni - imame syvpadenie s best subset
#Credit.Score,Maximum.Open.Credit,Term,perc,Annual.Income,,Current.Credit.Balance,Current.Loan.Amount,Purpose1,Monthly.Debt,Years.of.Credit.History,Number.of.Open.Accounts,,Months.since.last.delinquent1,Number.of.Credit.Problems1,Tax.Liens1,Home.Ownership1

sum(aux.lasso$abs.value)
aux.lasso$perc=round(aux.lasso$abs.value/sum(aux.lasso$abs.value),4)*100
aux.lasso 


# Random forest fit ----
#install.packages("randomForest")
library(randomForest)
eq6=randomForest(Loan.Status~.,data=dd,subset=train,importance=T,family=binomial)
windows()
varImpPlot(eq6,type=2)
eq6$importance[,1]/sum(eq6$importance[,1])

sapply(dds,class)

# xgboost fit  ----
dds=dd
ddx=dds[,names(dds)]
sapply(ddx,class)
ddx$Loan.Status=as.character(ddx$Loan.Status)
ddx = as.data.frame(sapply(ddx, as.numeric))
sapply(ddx,class) #all numeric
sm=sparse.model.matrix(Loan.Status~.,ddx)[,-1]
ddx
View(as.matrix(sm))
train1=sm[train,]
test1=sm[-train,]
label1=ddx$Loan.Status[train]
library(xgboost)
htrain=sample(round(nrow(train1)*7/8)) 
library(Matrix)
dtrain=xgb.DMatrix(train1[htrain,], label=label1[htrain]) 
dtest=xgb.DMatrix(train1[-htrain,], label=label1[-htrain])
dtest_f=xgb.DMatrix(test1, label=ddx$Loan.Status[-train])
# Fit the model
watchlist=list(train=dtrain, test=dtest) #70:10:20 
bst=xgb.train(data=dtrain, max_depth=2, eta=1, nthread = 2, nrounds=6, watchlist=watchlist, objective = "binary:logistic")
# Make predictions ----
label=getinfo(dtest, "label")
pred=predict(bst, dtest_f)
pred[1:10]
windows()
hist(pred)
err=as.numeric(sum(as.integer(pred > 0.5) != label))/length(label) 
print(paste("test-error=", err))

unique(label1)
table(label)
length(label)
table(label)/length(label)
length(pred)
pred.resp = ifelse(pred >= 0.5, 1, 0)
table(pred.resp) #

# Feature importance 
importance_matrix=xgb.importance(model = bst)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

# Validate results----
#Logistic regression
nn=data.frame(Loan.Status=dd$Loan.Status[-train],Estimated_Probability_eq2=predict(eq2,type="response",newdata = dd[-train,]))
windows()
hist(nn$Estimated_Probability_eq2,breaks = seq(from=0,to=1,by=0.05),col="light blue",main="Frequency distribution of estimated probabilities eq2")
nn$Predicted=ifelse(nn$Estimated_Probability_eq2>0.5,"Yes","No") # glm
table(nn$Loan.Status,nn$Predicted) #glm
(4017+536)/nrow(nn) #TRR 0.25

nn3=data.frame(Loan.Status=dd$Loan.Status[-train],Estimated_Probability_eq3=predict(eq3,type="response",newdata = dd[-train,]))
windows()
hist(nn3$Estimated_Probability_eq3,breaks = seq(from=0,to=1,by=0.05),col="light blue",main="Frequency distribution of estimated probabilities eq3")
nn3$Predicted=ifelse(nn3$Estimated_Probability_eq3>0.5,"Yes","No") # glm
table(nn3$Loan.Status,nn3$Predicted) #glm
(2714+56)/nrow(nn) #TRR 0.26

#Best Subset Logistic regression
nn1=data.frame(Loan.Status=dd$Loan.Status[-train],Estimated_Probability_eq20=predict(eq20,type="response",newdata = dd[-train,]))
windows()
hist(nn1$Estimated_Probability_eq20,breaks = seq(from=0,to=1,by=0.05),col="light blue",main="Frequency distribution of estimated probabilities eq20")
nn1$Predicted=ifelse(nn1$Estimated_Probability_eq20>0.5,"Yes","No") # glm
table(nn1$Loan.Status,nn1$Predicted) #glm
(2823+64)/nrow(nn1) #TRR 0.25

#LDA
eq4p=predict(eq4,dd[-train,])
nn4=data.frame(Loan.Status=dd$Loan.Status[-train],Estimated_Probability_eq4=eq4p$posterior[,2])
windows()
hist(nn4$Estimated_Probability_eq4,breaks = seq(from=0,to=1,by=0.05),col="light blue",main="Frequency distribution of estimated probabilities eq4")
eq4p=predict(eq4,dd[-train,]) # lda
table(dd$Loan.Status[-train],eq4p$class) #lda
(2875+7)/nrow(nn4) #TRR 0.22

#QDA
eq5p=predict(eq5,dd[-train,])
nn5=data.frame(Loan.Status=dd$Loan.Status[-train],Estimated_Probability_eq5=eq5p$posterior[,2])
windows()
hist(nn5$Estimated_Probability_eq5,breaks = seq(from=0,to=1,by=0.05),col="light blue",main="Frequency distribution of estimated probabilities eq5")
eq5p=predict(eq5,dd[-train,]) # qda
table(dd$Loan.Status[-train],eq5p$class) 
(2741+248)/nrow(nn5) #TRR 0.23

eq7p=predict(eq7,dd[-train,])
nn7=data.frame(Loan.Status=dd$Loan.Status[-train],Estimated_Probability_eq7=eq7p$posterior[,2])
windows()
hist(nn7$Estimated_Probability_eq7,breaks = seq(from=0,to=1,by=0.05),col="light blue",main="Frequency distribution of estimated probabilities eq7")
eq7p=predict(eq7,dd[-train,]) # qda
table(dd$Loan.Status[-train],eq7p$class) 
(40+10645)/nrow(nn5) #TRR 0.6

# Random forest
nn8=data.frame(Loan.Status=dd$Loan.Status[-train],Estimated_Probability_eq6=predict(eq6,type="prob",newdata = dd[-train,]))
windows() 
hist(treesize(eq6))
nn8$Loan.Status=ifelse(nn8$Loan.Status==1,"Yes","No") #Random forest
nn8$Estimated_Probability_eq6.1=ifelse(nn8$Estimated_Probability_eq6.1=="Yes",1,0)#Random forest
nn8$Predicted=ifelse(nn8$Estimated_Probability_eq6.1>0.5,"Yes","No") #Random forest
nn8$Predicted=ifelse(nn8$Estimated_Probability_eq6.0<0.5,"Yes","No") #Random forest
table(nn8$Loan.Status,nn8$Predicted) 
(2718+193)/nrow(nn5) #TRR 0.23
unique(nn8$Loan.Status)


