installed.packages("tidyverse")
library(tidyverse)
library(modelr)
library(ggplot2)
heights
wages<-heights%>%
  filter(income>0)
wages
summary(wages)
wages%>%
  ggplot(aes(income))+
  geom_density()
not_super_rich<-wages%>%
  filter(income<30000)
not_super_rich%>%
  ggplot(aes(income))+
  geom_density()
wages%>%
  filter(income>30000)%>%
  group_by(sex)%>%
  count()
not_super_rich%>%
  summarise(mean(income), median(income))
mean1<-mean(not_super_rich$income)
median1<-median(not_super_rich$income)

not_super_rich%>%
  ggplot(aes(income))+
  geom_histogram()+
  geom_vline(xintercept = c(mean1,median1), col=c("blue","red"))
quantile(not_super_rich$income, seq(0,1,by=0.1))
not_super_rich%>%
  ggplot(aes(education, color=sex))+
  geom_histogram()+
  facet_wrap(~sex)
wages%>%
  ggplot(aes(education))+
  geom_bar(position = "dodge")+
  facet_wrap(~sex)
lm(formula = not_super_rich$income~factor(not_super_rich$sex))
select(not_super_rich, sex)
lm(not_super_rich$income~factor(not_super_rich$sex))

men_income<-wages%>%
  filter(!is.na(income))%>%
  filter(!is.na(education))%>%
  group_by(income,education,sex)%>%
  select(contains("men"))
  
lm(formula = men_income$income~men_income$sex)         
#lm(men$income~men$education)
#women<-not_super_rich%>%
 # select(contains("female"), income, education)
#lm(women$income~women$education)
women_income<-wages%>%
  filter(!is.na(income))%>%
  filter(!is.na(education))%>%
  group_by(income,education, sex="female")%>%
  select(contains("women"))
lm(formula=women_income$income~women_income$sex)
not_super_rich
not_super_rich%>%
  ggplot(aes(x=sex, y=income, color=sex))+
  geom_line()
mpg  
mpg$manufacturer  
as.factor((mpg$manufacturer))
dim(mpg)  
trucks%>%
  lm(formula=log(price)~factor(make))
trucks%>%
  ggplot(aes(x=make, y=price))+
  geom_boxplot()
plot(log(price)~miles,data=trucks, col=factor(trucks$make))%>%
  legend("topright",fill=1:3, legend = levels(factor(trucks$make)))
plot(log(price) ~ miles, data = trucks, log = "y", col=factor(trucks$make), pch=16)
legend("topright", fill=1:3, legend=levels(factor(trucks$make)))
fit1<-lm(log(price)~miles, data = trucks)
summary(fit1)
iris$Sepal.Length
iris[["Sepal.Length"]]
iris["Sepal.Length"]
for (j in 1:ncol(iris)){
  if(is.numeric(iris[,j])){
    print(mean(iris[,j]))
  }
}
nyc <- list(pop = 8405837,
            boroughs = c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island"),
            capital = FALSE)
lapply(nyc,class)
sapply(nyc,class)
my_vector<-unlist(lapply(nyc, class))
my_vector
trucks

tr<-trucks%>%
  select(year,miles)
trucks%>%
  count(make)

