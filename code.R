###"""---Preparation steps--- """
mydata = read.csv(file="E:/itanic_newdata.csv",head=TRUE,sep=",")
names(mydata)
library(googleVis)
gvt = gvisTable(mydata, options = list(showRowNumber = T, height = 800,width=1200))
plot(gvt)
mydata[1:5,]
#mydata = mydata[, -c(1,13)] # ignore variables "Life_boat" and "train" at this moment. 
#gvt1 = gvisTable(mydata, options = list(showRowNumber = T, height = 800,width=1200))
#plot(gvt1) # google Visualization of the new dataset
mydata$name=as.character(mydata$name)
mydata$survived = as.factor(mydata$survived);
mydata$sex = as.factor(mydata$sex);
mydata$pclass = as.factor(mydata$pclass);
attach(mydata)
dim(mydata) # 1309*13
summary(mydata)
#sum(is.na(survived))  # "survived" for test data is missing
table(survived)
table(pclass)
table(sex)
#"""**************** Part I begins here. ****************"""
#"""----------Part(a)---------- """
# 3-Way Frequency Table
mytable = xtabs(~survived + pclass + sex, mydata);
ftable(mytable) 
#mytable[1,,] # survive=0
#mytable[2,,] # survive=1
# 5-number-summaries and histograms
fivenum(age, na.rm=T)
fivenum(fare, na.rm=T)
par(mfrow=c(1,2))
hist(age)
hist(fare)
# frequency tables for "sibsp" and "embarked"
ts = table(sibsp)
ts
te = table(embarked)
te
# Can you travel with zero "fare"?
0 %in% fare # return TRUE
length(mydata[!is.na(fare)&fare==0,1]) # return 17
# Who paid the most expensive ticket? 
m = max(fare, na.rm=T) # 512.3292
id = (1:1309)[!is.na(fare) & fare == m]
length(id)
a = 1;
maxfare <-array(0, dim=c(4,1))
for (i in 1 : 1309){
  if (!is.na(fare[i]) & fare[i] == m){
    maxfare[a] = name[i];
    a = a + 1;
  }
}
maxfare
# several families (> 6 family members) on board; who are they?
table(sibsp) 
table(parch) 
id = (1:1309)[sibsp + parch > 5]
length(id) # 33
fams <- array(0, dim=c(33,1))
a = 1;
for (i in 1 : 1309){
  if (sibsp[i] + parch[i] > 5){
    fams[a] = name[i];
    a = a + 1;
  }
}
fams
# big travel groups( > 5 people share the same ticket) Who are they?
length(levels(ticket)) # return 930
table(table(ticket))
tmp = table(ticket)
names(tmp)[tmp > 5] # 12 types of tickets
tmp = mydata[ticket %in% names(tmp)[tmp > 5],]
tmp[order(tmp$ticket),]$name # 86 Passengers
# Two missing values for variable "age"
mydata[is.na(age),] # 784 & 785
id = (1:1309)[(pclass==3) & (sex=='male')] 
length(id) # 493
a = mydata[id[order(name[id])], ]
median(a$age,na.rm=T) # 24
age[784] = 24
age[785] = 24
mydata[784,]$age = 24
mydata[785,]$age = 24 # Now we can use median(mydata$age)
# One missing value for variable "fare"
mydata[is.na(fare),] # 118
id2 = (1:1309)[pclass==3]
length(id2) # 709
b = mydata[id2[order(name[id2])], ]
#gvt3 = gvisTable(b, options = list(showRowNumber = T, height = 800,width=1200))
#plot(gvt3)
median(b$fare,na.rm=T) # 8.05
mydata[118,]$fare = 8.05 # Now we can use median(mydata$fare)
fare[118] = 8.05

#"""**************** Part II begins here. ****************"""
#"""----------Part(a)---------- """
# Divide the data into training and test. 
# Fit a logistic model survived ~ sex + pclass on the training data. 
# Fill in all missing values
mydata[784,]$age = 24
mydata[785,]$age = 24
mydata[118,]$fare = 8.05
train.data = mydata[train %in% 1, ]
test.data = mydata[train %in% 0, ]
g1 = glm(survived ~ sex + pclass, data = train.data, family = binomial(link="logit"))
drop1(g1,test="Chi")
# How to interprete the coefficients? 
summary(g1)$coeff
extractAIC(g1) # AIC 833.8884
extractAIC(g1, k=log(nrow(train.data))) # BIC 854.0577
# Summarize your prediction on the test data by a 2-by-2 table.
g1.pred = predict(g1, newdata = test.data, type="response")
g1.pred = as.numeric(g1.pred > 0.5)
table(g1.pred, test.data[,2])
# How is your prediction accuracy?
correct = sum(g1.pred == test.data[,2]) # 319
accuracy = correct/dim(test.data)[1]
accuracy # 76.32%

#"""----------Part(b)---------- """
# Fit a logistic model survived ~ sex*pclass on the training. 
g2 = glm(survived ~ sex * pclass, data = train.data, family = binomial(link="logit"))
drop1(g2,test="Chi")
# Compare model II(a) versus model II(b).
extractAIC(g2) #  AIC 810.0969
extractAIC(g2, k=log(nrow(train.data))) # BIC 838.851
# smaller AIC. The interaction model is more preferred than the additive model.
summary(g2)$coeff
# Summarize your prediction on the test data by a 2-by-2 table.
g2.pred = predict(g2, newdata = test.data, type="response")
g2.pred = as.numeric(g2.pred > 0.5)
table(g2.pred, test.data[,2])
# How is your prediction accuracy?
correct = sum(g2.pred == test.data[,2]) # 319
accuracy = correct/dim(test.data)[1]
accuracy # 76.32%

#"""----------Part(c)---------- """
# Add age to model II(b). Consider all interactions.
# 1.Original Model: survived ~ sex*pclass*age 
# without using step() or drop1()
g3 = glm(survived ~ sex*pclass*age,data = train.data, family = "binomial")
summary(g3)
extractAIC(g3) # AIC 780.4776
extractAIC(g3, k=log(nrow(train.data))) # BIC 837.9857
g3.pred = predict(g3, newdata = test.data, type="response")
g3.pred = as.numeric(g3.pred > 0.5)
table(g3.pred, test.data[,2])
# How is your prediction accuracy?
correct = sum(g3.pred == test.data[,2]) # 322
accuracy = correct/dim(test.data)[1]
accuracy # %77.03
# 2. Original Model: survived ~ sex*pclass*age 
# using step() or drop1() functions
g = step(g3)
#drop1(g3,test="Chi")
# survived ~ sex + pclass + age + sex:pclass + sex:age + pclass:age
g4 = glm(survived ~ sex + pclass + age + sex:pclass + sex:age + pclass:age, 
         data = train.data, family="binomial")
summary(g4)
extractAIC(g4) # AIC 779.3916 
extractAIC(g4, k=log(nrow(train.data))) # BIC 827.315
g4.pred = predict(g4, newdata = test.data, type="response")
g4.pred = as.numeric(g4.pred > 0.5)
table(g4.pred, test.data[,2])
# How is your prediction accuracy?
correct = sum(g4.pred == test.data[,2]) # 322
accuracy = correct/dim(test.data)[1]
accuracy # %77.03

# log-version of age use original log(age)
# 3. Original Model: survived ~ sex*pclass*log(age)
g3log = glm(survived ~ sex*pclass*log(age),data = train.data, family = "binomial")
summary(g3log)
extractAIC(g3log) # AIC 762.714
extractAIC(g3log, k=log(nrow(train.data))) # BIC 820.2221
g = step(g3log)
g3log.pred = predict(g3log, newdata = test.data, type="response")
g3log.pred = as.numeric(g3log.pred > 0.5)
table(g3log.pred, test.data[,2])
# How is your prediction accuracy?
correct = sum(g3log.pred == test.data[,2]) # 325
accuracy = correct/dim(test.data)[1]
accuracy # %77.75
# log-version of age not use original log(age)
g4log = glm(survived ~ sex + pclass + log(age) + sex:pclass + sex:(log(age)) + pclass:(log(age)), 
         data = train.data, family="binomial")
summary(g4log)
extractAIC(g4log) # AIC 767.0838
extractAIC(g4log, k=log(nrow(train.data))) # BIC 815.0073
g4log.pred = predict(g4log, newdata = test.data, type="response")
g4log.pred = as.numeric(g4log.pred > 0.5)
table(g4log.pred, test.data[,2])
# How is your prediction accuracy?
correct = sum(g4log.pred == test.data[,2]) # 325
accuracy = correct/dim(test.data)[1]
accuracy # %77.75
#"""----------Part(d)---------- """
mydata$embarked = as.factor(mydata$embarked);
g5 = glm(survived ~ sex*pclass*log(age)+fare+embarked+sibsp+parch, 
         data = train.data, family="binomial")
summary(g5)
extractAIC(g5) # AIC 736.6964
extractAIC(g5, k=log(nrow(train.data))) # BIC 822.9586
#********Use AIC criterion to step()
g = step(g5, k = 2)  
# AIC survived~sex+pclass+log(age)+embarked+sibsp+sex:pclass+
# sex:log(age)+pclass:log(age)+sex:pclass:log(age) 
g6 = glm(survived ~ sex + pclass + log(age) + embarked + sibsp + sex:pclass + 
           sex:log(age) + pclass:log(age) + sex:pclass:log(age), 
         data = train.data, family="binomial")
summary(g6)
extractAIC(g6) # 734.7276
extractAIC(g6, k=log(nrow(train.data))) # BIC 811.4051
g6.pred = predict(g6, newdata = test.data, type="response")
g6.pred = as.numeric(g6.pred > 0.5)
table(g6.pred, test.data[,2])
# How is your prediction accuracy?
correct = sum(g6.pred == test.data[,2]) # 329
accuracy = correct/dim(test.data)[1]
accuracy # 78.71%
#********Use BIC criterion to step() 
g= step(g5, k = log(nrow(train.data))) # BIC
# BIC
# survived~sex+pclass+log(age)+sibsp+sex:pclass+sex:log(age)
g7 = glm(survived~sex+pclass+log(age)+sibsp+sex:pclass+sex:log(age), 
         data = train.data, family="binomial")
summary(g7)
extractAIC(g7) # AIC 741.4095
extractAIC(g7, k=log(nrow(train.data))) # BIC 784.5406
g7.pred = predict(g7, newdata = test.data, type="response")
g7.pred = as.numeric(g7.pred > 0.5)
table(g7.pred, test.data[,2])
# How is your prediction accuracy?
correct = sum(g7.pred == test.data[,2]) # 324
accuracy = correct/dim(test.data)[1]
accuracy # 77.51%
#"""----------Part(e)---------- """
# g6 in II(d) gives the smallest AIC, 734.7276.
# g6 in II(d) gives the highest accuracy, 78.71%
AIC <- c(833.8884,810.0969,780.4775,779.3916,
         762.714,767.0838,736.6964,734.7276,741.4095)
BIC <- c(854.0577,838.851,837.9857,827.315,820.2221,
         815.0073,822.9586,811.4051,784.5406)
sort(AIC)
sort(BIC)
#"""**************** Part III begins here. ****************"""
#Explore various tree models with all the variables except 
#Life_boat, name, ticket and cabin.
library(rpart)
drops=c("Life_boat", "name", "ticket", "cabin")
train.data = train.data[, !names(train.data) %in% drops]
test.data = test.data[, !names(test.data) %in% drops]
g.tree1 = rpart(survived ~ ., data=train.data,minsplit=5, cp=0.000001,maxdepth=30,method="class")
plotcp(g.tree1)
printcp(g.tree1) 
g.tree1$cptable[which.min(g.tree1$cptable[,"xerror"]),"CP"]
# 0.00389864<x<0.00438596 nsplit=14  
g.tree2 = rpart(survived ~ ., data=train.data,minsplit=5, cp=0.00389864,method="class")
plotcp(g.tree2)
printcp(g.tree2) 
g.tree2$cptable[which.min(g.tree2$cptable[,"xerror"]),"CP"]
# 0.0077973<x<0.0204678 nsplit=6
g.tree3 = rpart(survived ~ ., data=train.data,minsplit=5, cp=0.007797271)
plotcp(g.tree3)
printcp(g.tree3) 
g.tree3$cptable[which.min(g.tree3$cptable[,"xerror"]),"CP"]
round(printcp(g.tree3),dig=4)
gt = prune.rpart(g.tree3, 0.008)
plot(gt,compress=T,uniform=T,branch=0.5,margin=0.05)
text(gt,cex=0.8,font=2,use.n=T, all=T)
g8.pred = predict(gt, newdata = test.data,"class")
table(g8.pred, test.data[,1])
# How is your prediction accuracy?
correct = sum(g8.pred == test.data[,1]) # 326
accuracy = correct/dim(test.data)[1]
accuracy # 77.99%  
tree1=prune.rpart(g.tree,0.007797271)
length(mydata[mydata[,5]=="male"&mydata[,13]==1,1])
# Understanding of the tree.....
dim(train.data[train.data$survived==1,])#342
dim(train.data[train.data$train==1&train.data$sex=="female"
              &train.data$survived==1,])#233
dim(train.data[train.data$sex=="male"&train.data$survived==1,])#109
dim(train.data[train.data$survived==0,])#549
dim(train.data[train.data$sex=="female"&train.data$survived==0,])#81
dim(train.data[train.data$sex=="male"&train.data$survived==0,])#468
dim(train.data[train.data$age>=12.5&train.data$sex=="male"&train.data$survived==0,])#450
dim(train.data[train.data$age<12.5&train.data$sex=="male"&train.data$survived==0,])#18
dim(train.data[train.data$age>=12.5&train.data$sex=="male"&train.data$survived==1,])#86
dim(train.data[train.data$age<12.5&train.data$sex=="male"&train.data$survived==1,])#123
dim(train.data[train.data$age>=12.5&train.data$sex=="male"
    &train.data$survived==1&train.data$sibsp>=2.5,])#0
dim(train.data[train.data$age<12.5&train.data$sex=="male",])#41
dim(train.data[train.data$sex=="female"&train.data$survived==1&train.data$pclass!=3,])#161
# random forest
library(randomForest)
forest<-randomForest(survived ~ ., data=train.data,
                     proximity=T,importance=TRUE,ntrees=500,method="class")
g9.pred<-predict(forest,test.data,)
table(g9.pred,test.data$survived)
correct = sum(g9.pred == test.data[,1]) 
accuracy = correct/dim(test.data)[1]
accuracy #77.27%
