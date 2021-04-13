installed.packages("tidyverse")
library(tidyverse)
library(ggplot2)
iris
count(iris)
density(iris$Sepal.Width)
ggplot(data= iris, aes(x= Sepal.Width))+geom_bar()
mpg
ggplot(data= mpg, aes(x= manufacturer, fill=trans))+geom_bar()
ggplot(data= mpg, aes(x= trans, fill=manufacturer))+geom_bar()
mpg

ggplot(data=mpg, aes(x= displ, y= manufacturer))+geom_point(color="black")
mpg
ggplot(data=mpg, aes(x=displ, y=hwy, color=manufacturer)) + geom_point()
ggplot(data=iris, aes(x= Sepal.Length,y= Sepal.Width,color=Species)) + geom_line()+facet_grid(.~Species)
install.packages("nycflights13")
library(nycflights13)
flights$carrier
flights$dep_delay
dplyr
flights %>%
filter(distance>1000, dep_delay==0, carrier=="DL", arr_delay>30 )%>%
  filter(!is.na(dep_time))%>%
  arrange(desc(dep_time))
iris%>%
  group_by(Species)%>%
  summarise(mean(Sepal.Length))

flights%>%
  filter(!is.na(arr_delay) & !is.na(distance))%>%
  group_by(dest)%>%
  summarise(mean(arr_delay), mean(distance))

ggplot(data=flights, aes(x=mean(arr_delay, na.rm = TRUE),y=mean(distance, na.rm = TRUE)) + geom_point()
?who_disease
library(modelr)
wages<-heights%>%
  filter(income>0)
wages
summary(wages)
head(wages)
wages%>%
  ggplot(aes(log(income)))+ geom_histogram()
wages %>% 
  filter(income > 300000) %>% 
  group_by(sex) %>% 
  count()
mean1<-mean(wages$income)
median1<-median(wages$income)
wages %>%
  ggplot(aes(income))+
  geom_histogram()+
  geom_vline(xintercept = c(mean1,median1),col=c("red","blue"))
wages %>% 
  ggplot(aes(income)) +
  geom_histogram() +
  geom_vline(xintercept=c(mean1, median1), col=c("red", "blue"))
wages%>%
  arrange(desc(income))
mean1<-(wages$income<30000)
wages[mean1,]
df<-wages%>%
  filter(income<30000)
df
df%>%
  ggplot(aes(x=income))+
  geom_histogram()+
  geom_vline(xintercept = c(mean(df$income), median(df$income)), col=c("red","blue"))
  
df%>%
  summarise(mean(income),median(income))

quantile(df$income, seq(0,1,0.1))
sum(is.na(df))
df%>%
  ggplot(aes(education))+ geom_bar()+facet_wrap(~sex)
df%>%
  ggplot(aes(x=education, fill= sex))+ geom_bar(position="dodge")+labs(title="Education in Years", x="Years", y="Count")

summary(df$education)
quantile(df$education, na.rm=TRUE)

df%>%
  filter(!is.na(income), !is.na(education))%>%
  summarise(correlation=cor(income,education))
cor.test(df$income,df$education)

df%>%
  ggplot(aes(income, education, color=sex))+geom_line()+facet_wrap(~sex)+labs(title = "Relationship between income and educaation", subtitle = "after filtering out super rich people",x="Income", y="Education")

as.integer(runif(10,0,10))
x<-seq(0,5)
x[-c(2,3)]
iris
iris[,2]


  cor(diamonds$price, diamonds$carat)
mpg
cor(mpg$displ, mpg$hwy )

summary(mpg)
factor(mpg$manufacturer, order=TRUE)
unemploy_rate
economics%>%
  mutate(economics,unemploy_rate)
mpg
ggplot(mpg, aes(displ))+ geom_bar()
ggplot(mpg, aes(x=hwy, y=displ))+geom_smooth(method="loess")
flights%>%
  filter(month==11,day==13)%>%
  select(dep_delay)
select(flights, year:dep_time)
select(flights, -c(dep_delay:arr_delay))
select(flights,distance,everything())%>%
  arrange(desc(distance))
select(flights, contains("time"))
select(flights, starts_with("dep"))
select(flights, ends_with("delay"))
mutate(flights, regain=arr_delay-dep_delay)%>%
  select(regain,everything())

regain_df<-flights%>%
  transmute(regain= arr_delay-dep_delay,regain_hours= regain/60,distance)
regain_df
regain_df%>%
  ggplot(aes(regain))+geom_histogram()
flights%>%
  summarise(mean(dep_time, na.rm = TRUE), mean(arr_time, na.rm=TRUE), IQR(dep_time, na.rm = TRUE))
flights%>%
  group_by(year,month,day)%>%
  summarise(mean(dep_delay, na.rm=TRUE ), IQR(dep_delay, na.rm=TRUE))
flights%>%
  group_by(year,month,day)%>%
  filter(!is.na(dep_time))%>%
  summarise(mean_dep_time=mean(dep_time))%>%
  mutate(delay_hours= mean_dep_time/60)%>%
  ggplot(aes(delay_hours))+
  geom_density()+
  labs(title="Departure Delay", subtitle = "desity(in hours)")
mpg%>%
  plot(lm(formula = displ~factor(manufacturer)+hwy+cty))
  
plot(lm(formula = mpg$displ~mpg$manufacturer))
library(modelr)

library(gapminder)
gapminder
summary(gapminder)

  
ggplot(data= gapminder, aes(x=gdpPercap,y=lifeExp, color=continent))+geom_point()+scale_color_manual(values = c("blue", "green", "yellow","black","violet"))
getwd()
