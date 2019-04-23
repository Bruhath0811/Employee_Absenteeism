rm(list=ls())
setwd("E:/R/Proect2 files")
getwd()
#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees',"readxl")
install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)
library("readxl")
Ab_data=read_xls("Employee_absenteeism.xls",col_names = TRUE,na ="")
colnames(Ab_data)
library(stringr)
colnames(Ab_data)<-str_replace_all(colnames(Ab_data), c(" " = "_","/" = "_"))
colnames(Ab_data)
str(Ab_data)
View(Ab_data)
Ab_data$ID=as.factor(Ab_data$ID)
Ab_data$Reason_for_absence=as.factor(Ab_data$Reason_for_absence)
Ab_data$Month_of_absence=as.factor(Ab_data$Month_of_absence)
Ab_data$Day_of_the_week=as.factor(Ab_data$Day_of_the_week)
Ab_data$Seasons=as.factor(Ab_data$Seasons)
Ab_data$Disciplinary_failure=as.factor(Ab_data$Disciplinary_failure)
Ab_data$Education=as.factor(Ab_data$Education)
Ab_data$Social_drinker=as.factor(Ab_data$Social_drinker)
Ab_data$Social_smoker=as.factor(Ab_data$Social_smoker)
Ab_data$Son=as.factor(Ab_data$Son)
Ab_data$Pet=as.factor(Ab_data$Pet)
str(Ab_data)
#Missing value Analysis 
Missing_val=data.frame(apply(Ab_data,2,function(X){sum(is.na(X))}))
Missing_val$columns=row.names(Missing_val)
names(Missing_val)[1] =  "Missing_percentage"
Missing_val$Missing_percentage=(Missing_val$Missing_percentage/nrow(Ab_data))*100
Missing_val = Missing_val[order(-Missing_val$Missing_percentage),]
View(Missing_val)
row.names(Missing_val)=NULL
Missing_val = Missing_val[,c(2,1)]

sum(is.na(Ab_data$Body_mass_index))

Ab_data[368,2]=0
Ab_data[374,2]=0
Ab_data[381,2]=0
Ab_data[67,3]=2

View(Ab_data)


Ab_data$`Work_load_Average_day`[is.na(Ab_data$`Work_load_Average_day`)]=mean(Ab_data$`Work_load_Average_day`,na.rm = T)
Ab_data$Hit_target[is.na(Ab_data$Hit_target)]=mean(Ab_data$Hit_target,na.rm = T)

Ab_data$Absenteeism_time_in_hours[is.na(Ab_data$Absenteeism_time_in_hours)]=mean(Ab_data$Absenteeism_time_in_hours,na.rm = T)

sum(is.na(Ab_data))

###outllier analsis

numeric_index = sapply(Ab_data,is.numeric)
numeric_data = Ab_data[,numeric_index]
cnames = colnames(numeric_data)

 for (i in 1:length(cnames))
 {
   assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "Absenteeism_time_in_hours"), data = subset(Ab_data))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                         outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="Absenteeism_time_in_hours")+
           ggtitle(paste("Box plot of Absenteeism_time_in_hrs for",cnames[i])))
 }


gridExtra::grid.arrange(gn1,gn10,ncol=2)
gridExtra::grid.arrange(gn6,gn7,ncol=2)
gridExtra::grid.arrange(gn8,gn9,ncol=2)
df = Ab_data
Ab_data=df
for(i in cnames){
  print(i)
  val = Ab_data[[i]][Ab_data[[i]] %in% boxplot.stats(Ab_data[[i]])$out]
  print(length(val))
  Ab_data = Ab_data[which(!Ab_data[[i]] %in% val),]
}


##coorreation Plot

corrgram(Ab_data[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

## Anova Test of Independence


#factor_index = sapply(Ab_data,is.factor)
#factor_data = Ab_data[,factor_index]

print(summary(aov(Absenteeism_time_in_hours~ID,data = Ab_data)))
print(summary(aov(Absenteeism_time_in_hours~Reason_for_absence,data = Ab_data)))
print(summary(aov(Absenteeism_time_in_hours~Month_of_absence,data = Ab_data)))
print(summary(aov(Absenteeism_time_in_hours~Day_of_the_week,data = Ab_data)))
print(summary(aov(Absenteeism_time_in_hours~Seasons,data = Ab_data)))
print(summary(aov(Absenteeism_time_in_hours~Disciplinary_failure,data = Ab_data)))
print(summary(aov(Absenteeism_time_in_hours~Education,data = Ab_data)))
print(summary(aov(Absenteeism_time_in_hours~Social_drinker,data = Ab_data)))
print(summary(aov(Absenteeism_time_in_hours~Social_smoker,data = Ab_data)))
print(summary(aov(Absenteeism_time_in_hours~Son,data = Ab_data)))
print(summary(aov(Absenteeism_time_in_hours~Pet,data = Ab_data)))



Ab_data = subset(Ab_data,select = -c(Day_of_the_week,Social_smoker,Education,Seasons,Body_mass_index,Service_time))

##Feature scaling

View(Ab_data)
unique(Ab_data$Transportation_expense)
qqnorm(Ab_data$Transportation_expense)
hist(Ab_data$Transportation_expense)
hist(Ab_data$Distance_from_Residence_to_Work)
hist(Ab_data$Service_time)

hist(Ab_data$`Work_load_Average/day`)
hist(Ab_data$Hit_target)
hist(Ab_data$Weight)
hist(Ab_data$Height)
hist(Ab_data$Absenteeism_time_in_hours)

View(Ab_data)

str(Ab_data)

#Normalization  
cnrnames = c("Transportation_expense","Age","Weight","Absenteeism_time_in_hours","Distance_from_Residence_to_Work","Work_load_Average_day","Hit_target","Height")
  
  for(i in cnrnames){
  print(i)
  Ab_data[,i] = (Ab_data[,i] - min(Ab_data[,i]))/
    (max(Ab_data[,i] - min(Ab_data[,i])))
  }

View(Ab_data)

sum(is.na(Ab_data))
    
##random Sampling Technique

Train_index=sample(1:nrow(Ab_data),0.8*nrow(Ab_data))

train = Ab_data[Train_index,]
colnames(train)

test = Ab_data[-Train_index,]

View(train)


##Modelling 
#rm(RF_model)

RF_model = randomForest(Absenteeism_time_in_hours ~ ., train, importance = TRUE, ntree = 500)


treelist=RF2List(RF_model)

exec=extractRules(treelist,train[,-15])

exec[1:5,]

readableRules = presentRules(exec,colnames(train))
readableRules[1:10,]

ruleMetric=getRuleMetric(exec,train[,-15], train$Absenteeism_time_in_hours)

RF_predictions = predict(RF_model,test[,-15])

View(test)

View(RF_predictions)


rmse <- function(error)
{
  sqrt(mean(error^2))
}

error <- test[,15] - RF_predictions

rmse(data.matrix(error))

##rmse(error)

print(RF_model)

library(DMwR)
regr.eval(test[,15],RF_predictions,stats = c('mae','rmse','mape','mse'))



##Decision tree
fit= rpart::rpart(Absenteeism_time_in_hours ~ .,data=train,method = "anova" )

predictions_DT = predict(fit,test[,-15])

regr.eval(test[,15],predictions_DT,stats = c("mae","mape","rmse","mse"))


#Linear regression

install.packages("usdm")
library(usdm)
vif(Ab_data[,-15])

vifcor(Ab_data[,-15],th=0.9)

lm_model=lm(Absenteeism_time_in_hours ~ .,data=train)

View(test)

test

predictions_LR=predict(lm_model,test[,-15])

summary(lm_model)




