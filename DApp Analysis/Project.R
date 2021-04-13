library(reshape2)
library(tidyverse)
library(ggplot2)
library(dplyr) 
dapp<- read.csv("radar.csv")
summary(dapp)



for(i in 1:length(dapp$Txs_24h)){
  if(grepl('[.,]',dapp$Txs_24h[i])){
    dapp$Txs_24h[i]<-  gsub('[.,]', '', dapp$Txs_24h[i])
    dapp$Txs_24h[i]<-  gsub('[k,]', '00', dapp$Txs_24h[i])
    dapp$Txs_24h[i]<-  gsub('[M,]', '00000', dapp$Txs_24h[i])
  }
  else{
    dapp$Txs_24h[i]<-  gsub('[k,]', '000', dapp$Txs_24h[i])
    dapp$Txs_24h[i]<-  gsub('[M,]', '000000', dapp$Txs_24h[i])
  }

}
dapp$Txs_24h<-as.numeric(dapp$Txs_24h)

for(i in 1:length(dapp$Txs_7d)){
  if(grepl('[.,]',dapp$Txs_7d[i])){
    dapp$Txs_7d[i]<-  gsub('[.,]', '', dapp$Txs_7d[i])
    dapp$Txs_7d[i]<-  gsub('[k,]', '00', dapp$Txs_7d[i])
    dapp$Txs_7d[i]<-  gsub('[M,]', '00000', dapp$Txs_7d[i])
  }
  else{
    dapp$Txs_7d[i]<-  gsub('[k,]', '000', dapp$Txs_7d[i])
    dapp$Txs_7d[i]<-  gsub('[M,]', '000000', dapp$Txs_7d[i])
  }
  
}

dapp$Txs_7d<-as.numeric(dapp$Txs_7d)
median(dapp$Txs_7d)


for(i in 1:length(dapp$balance)){
  if(grepl('[.,]',dapp$balance[i])){
    dapp$balance[i]<-  gsub('[.,]', '', dapp$balance[i])
    dapp$balance[i]<-  gsub('[k,]', '00', dapp$balance[i])
    dapp$balance[i]<-  gsub('[M,]', '00000', dapp$balance[i])
  }
  else{
    dapp$balance[i]<-  gsub('[k,]', '000', dapp$balance[i])
    dapp$balance[i]<-  gsub('[M,]', '000000', dapp$balance[i])
  }
  
}

dapp$balance<-as.numeric(dapp$balance)
summary(dapp$balance)


for(i in 1:length(dapp$volume_24h)){
  if(grepl('[.,]',dapp$volume_24h[i])){
    dapp$volume_24h[i]<-  gsub('[.,]', '', dapp$volume_24h[i])
    dapp$volume_24h[i]<-  gsub('[k,]', '00', dapp$volume_24h[i])
    dapp$volume_24h[i]<-  gsub('[M,]', '00000', dapp$volume_24h[i])
    dapp$volume_24h[i]<-  gsub('[$,]', '', dapp$volume_24h[i])
  }
  else{
    dapp$volume_24h[i]<-  gsub('[k,]', '000', dapp$volume_24h[i])
    dapp$volume_24h[i]<-  gsub('[M,]', '000000', dapp$volume_24h[i])
    dapp$volume_24h[i]<-  gsub('[$,]', '', dapp$volume_24h[i])
  }
  
}

dapp$volume_24h<-as.numeric(dapp$volume_24h)
summary(dapp$volume_24h)

for(i in 1:length(dapp$volume_7d)){
  if(grepl('[.,]',dapp$volume_7d[i])){
    dapp$volume_7d[i]<-  gsub('[.,]', '', dapp$volume_7d[i])
    dapp$volume_7d[i]<-  gsub('[k,]', '00', dapp$volume_7d[i])
    dapp$volume_7d[i]<-  gsub('[M,]', '00000', dapp$volume_7d[i])
    dapp$volume_7d[i]<-  gsub('[$,]', '', dapp$volume_7d[i])
  }
  else{
    dapp$volume_7d[i]<-  gsub('[k,]', '000', dapp$volume_7d[i])
    dapp$volume_7d[i]<-  gsub('[M,]', '000000', dapp$volume_7d[i])
    dapp$volume_7d[i]<-  gsub('[$,]', '', dapp$volume_7d[i])
  }
  
}
dapp$volume_7d<-as.numeric(dapp$volume_7d)
summary(dapp$volume_7d)

for(i in 1:length(dapp$users_24h)){
  if(grepl('[.,]',dapp$users_24h[i])){
    dapp$users_24h[i]<-  gsub('[.,]', '', dapp$users_24h[i])
    dapp$users_24h[i]<-  gsub('[k,]', '00', dapp$users_24h[i])
    dapp$users_24h[i]<-  gsub('[M,]', '00000', dapp$users_24h[i])

  }
  else{
    dapp$users_24h[i]<-  gsub('[k,]', '000', dapp$users_24h[i])
    dapp$users_24h[i]<-  gsub('[M,]', '000000', dapp$users_24h[i])
   
  }
  
}

dapp$users_24h<-as.numeric(dapp$users_24h)
summary(dapp$users_24h)
features <- c("Txs_24h","Txs_7d", "balance", "volume_24h", "volume_7d","users_24h")
cormat <- round(cor(dapp[c("rank", features)]), 2)

melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
cormat

ggplot(dapp,aes(users_24h))+ geom_histogram()
library(olsrr)
library(car)
mc<-car::vif(lm(data=dapp, rank~Txs_24h+volume_24h+users_24h))
mc



state_of_dapp<-read.csv("state_of_the_dapp.csv")
state<-state_of_dapp%>%
  arrange(rank)

for(i in 1:length(state$volume_7d)){
  if(grepl('[USD,]',state$volume_7d[i])){
    state$volume_7d[i]<-  gsub('[USD,]', '', state$volume_7d[i])

  }
  
}
state
state$volume_7d
summary(state)
state$dev_activity_30d<-as.numeric(state$dev_activity_30d)
state$volume_7d<-as.numeric(state$volume_7d)
state$users_24h<-as.numeric(state$users_24h)
features <- c("dev_activity_30d","transaction_count", "total_transaction_volume_ether", "volume_7d","users_24h","contract_count","dapp_total_loss","user_loss_average")
cormat <- round(cor(state[c("rank", features)],use="complete.obs"), 2)

melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
cormat

dapp_filter_zero<-dapp%>%
  filter(users_24h>0)%>%
  filter(Txs_24h>0)%>%
  filter(volume_24h>0)%>%
  filter(balance>0)%>%
  filter(balance<25000)

plot(lm(data=dapp_filter_zero, rank~users_24h+volume_24h+Txs_24h+balance))
model1<-lm(data=dapp_filter_zero, rank~poly(users_24h, degree=3))
plot(model1)
state_filter_zero<-state%>%
  filter(users_24h>0)%>%
  
  arrange(desc(users_24h))
  
summary(lm(data=state_filter_zero, users_24h~dev_activity_30d+transaction_count+contract_count+dapp_total_loss+user_loss_average))


ggplot(data = dapp_filter_zero, aes(users_24h,Txs_24h))+ geom_point()
ggplot(data = dapp_filter_zero, aes(users_24h,balance))+ geom_point()
ggplot(data = dapp_filter_zero, aes(Txs_24h,volume_24h))+ geom_point()
ggplot(data = dapp_filter_zero, aes(rank,users_24h))+ geom_point()
ggplot(data = dapp_filter_zero, aes(rank,Txs_24h))+ geom_point()
ggplot(data = dapp_filter_zero, aes(rank,volume_24h))+ geom_point()
ggplot(data = dapp_filter_zero, aes(rank,users_24h))+ geom_point()
ggplot(data = state_filter_zero, aes(rank,users_24h))+ geom_point()
ggplot(data = state_filter_zero, aes(users_24h,transaction_count))+ geom_point()

ggplot(data = dapp, aes(users_24h,Txs_24h))+ geom_point()
ggplot(data = dapp, aes(users_24h,volume_24h))+ geom_point()
ggplot(data = dapp, aes(Txs_24h,volume_24h))+ geom_point()
ggplot(data = dapp, aes(rank,users_24h))+ geom_point()
ggplot(data = dapp, aes(rank,Txs_24h))+ geom_point()
ggplot(data = dapp, aes(rank,volume_24h))+ geom_point()
ggplot(data = dapp, aes(rank,users_24h))+ geom_point()

cor(state$dapp_total_loss,state$user_loss_average)
cor(state$dapp_total_loss,state$user_total_loss)

dapp<- state%>%
  filter(dapp_total_loss!=(-1.0000))%>%
  filter(user_total_loss!=(-1.0000))%>%
  filter(user_loss_average!=(-1.0000))%>%
  filter(total_transaction_volume_ether!=(-1.0000))

library(olsrr)
plot(lm(data= dapp, dapp_total_loss~total_transaction_volume_ether+user_loss_average+user_count_unique_remove_contract_creator))
plot(lm(data= state, dapp_total_loss~poly(total_transaction_volume_ether, degree=2)+ poly(user_loss_average, degree=2)+poly(user_count_unique_remove_contract_creator, degree=2)))
plot(lm(data= state, dapp_total_loss~poly(total_transaction_volume_ether, degree=3)+ poly(user_loss_average, degree=3)+poly(user_count_unique_remove_contract_creator, degree=3)))
plot(lm(data= state, dapp_total_loss~poly(total_transaction_volume_ether, degree=4)+ poly(user_loss_average, degree=4)+poly(user_count_unique_remove_contract_creator, degree=4)))
summary(lm(data= state, dapp_total_loss~poly(total_transaction_volume_ether, degree=5)+ poly(user_loss_average, degree=5)+poly(user_count_unique_remove_contract_creator, degree=5)))

dist<-count(state_of_dapp, "dapp_total_loss")


library(caret)

classification<- state%>%
  filter(dapp_total_loss!=(-1.0000))%>%
  filter(user_total_loss!=(-1.0000))%>%
  filter(user_loss_average!=(-1.0000))%>%
  filter(total_transaction_volume_ether!=(-1.0000))

dis1<-as.factor(classification$user_total_loss)
dis<-unique(classification$dapp_total_loss)

count(classification,"dapp_total_loss")

c
d
e
library("dplyr")
for (i in 1:length(classification$dapp_total_loss)){
  if(classification$dapp_total_loss[i] >0.000000e+00){
    classification$dapp_total_loss[i]<- 1
  }
  else if(classification$dapp_total_loss[i] == 0.000000e+00){
    classification$dapp_total_loss[i]<- 0
  }
  else if(classification$dapp_total_loss[i] < 0.000000e+00){
    classification$dapp_total_loss[i]<- 0
  }
  
}
class(classification$dapp_total_loss)
c=0
d=0
e=0
for(i in 1:length(classification$dapp_total_loss)){
  if(classification$dapp_total_loss[i]=="1"){
    c=c+1
  }
  if(classification$dapp_total_loss[i]=="0"){
    d=d+1
  }
  if(classification$dapp_total_loss[i]=="-1"){
    e=e+1
  }
}

factor(classification$dapp_total_loss)
classification$dapp_total_loss<-as.numeric(classification$dapp_total_loss)
training_index<-createDataPartition(classification$dapp_total_loss,p=0.8, list=FALSE )
training_set<-classification[training_index,]
testing_set<-classification[-training_index,]

factor(classification$dapp_total_loss)



library(caret)
library(e1071)
model1<-train(dapp_total_loss~total_transaction_volume_ether+user_loss_average+user_count_unique_remove_contract_creator+user_total_loss,data=training_set,
              method="svmPoly",
              na.action=na.omit,
              preProcess=c("scale","center"),
              trControl=trainControl(method="none"),
              tuneGrid=data.frame(degree=1,scale=1,C=1)
              )

model.cv<-train(dapp_total_loss~total_transaction_volume_ether+user_loss_average+user_count_unique_remove_contract_creator+user_total_loss,data=training_set,
              method="svmPoly",
              na.action=na.omit,
              preProcess=c("scale","center"),
              trControl=trainControl(method="cv", number = 10),
              tuneGrid=data.frame(degree=1,scale=1,C=1)
)

model1<-predict(training_set$dapp_total_loss)
class(training_set$dapp_total_loss)
Model.training<- predict(model1,training_set)
Model.testing<-predict(model1,testing_set)
Model.cv<-predict(model.cv, training_set)


model.training.confusion<-confusionMatrix(table(Model.training, training_set$dapp_total_loss))
model.testing.confusion<-confusionMatrix(table(Model.testing, testing_set$dapp_total_loss))
#model.cv.confusion<-confusionMatrix(model.cv, training_set$dapp_total_loss)

print(model.training.confusion)
print(model.testing.confusion)

plot(model.taining.confusion)
