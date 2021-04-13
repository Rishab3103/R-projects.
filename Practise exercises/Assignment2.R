install.packages("nycflights13")
library(nycflights13)
print(flights)
head(flights)
class(flights))
  
tail(flights)
nrow(flights)
ncol(flights)
flights$dep_time
flights$distance[1]
median(flights$distance)
nrow(flights$dep_delay)
q<-sum(as.numeric(flights$distance>mean(flights$distance)))
m<-c()
for (i in 1:length(flights$dep_delay)) {
  if(flights$dep_delay[i]>20){
    m<-c(flights$distance[i])
    mean_distance=mean(m)

  }
}
print(mean_distance)
sum1= sum(as.numeric(is.na(flights$dep_time)))
flights$dep_delay
flights?month

a<-rnorm(n=10, mean = 4, sd=3)
b<-rnorm(n=10, mean = 3, sd=4)
c<-rnorm(n=10, mean = 6, sd=2)
d<-rnorm(n=10, mean = 4, sd=3)
df<-data.frame(a,b,c,d)
v<-c(a,b,c,d)
z<-c()
df
for(i in 1:10){
  for(j in 1:4){
  
    
  
    z[i]<-c(median((df[i,j])))
  
  
}  
}

print(z)
lapply(v, median)

cities <- list("Barbados", "Sankt Augustin", "Aachen", "Cologne")
lapply(cities,nchar)

getwd()
dir()
web<- read.csv("web-browsers.csv")
class(variable.names(web))
nrow(web)
length(web$hispanic)
web
p<-(sum(web$broadband)/10000)*100
print(p)
mean1<-mean(web$anychildren)
print(mean1)
mean2<-(10000-sum(web$anychildren))/10000
print(mean2)
web$spend
sd(web$spend)
max(web$spend)
min(web$spend)
mean(web$spend)
median(web$spend)
quantile(web$spend)
hist((web$spend))
hist(log(web$spend))
plot(log(spend)~ factor(anychildren), data = web)
plot(lm(log(spend)~ hispanic,data = web))
return

install.packages("dslabs")
library(dslabs)
?murders
print(murders)
class(murders$state)
murders$region["Levels"]
mean(murders$population)
murders$population
sum_pop= sum(as.numeric((murders$population*3)>mean(murders$population)))
states=c()
for (i in 1:length(murders$population)) {
  if((murders$population[i]*3)>mean(murders$population)){
    states[i]=c(murders$state[i])
  }
  
}
print(states)
states1=c()
high=c()
for(i in 1:5)
{
  states1[i]=c(murders$state[i])
  if(murders$total[i]>mean(murders$total))
  {
  high[i]= (states1[i])
  }
  }

print(states1)
print(high)
murders$total
murders$state
table(murders$state)
sort(murders$total)
z<-c()
a<-c(murders$total)
z<-c(murders$state)
names(z)<-a

sort(a)
which.max(a)
which.min(a)
rate<-c()
murders$rate<-rate
for (i in 1:length(murders$population)) 
  murders$rate[i]=(murders$total[i]/murders$population[i])*100

print(murders)

st<-c()
for(i in 1:length(murders$rate)){
  if(murders$rate[i]<0.42)
  
    st[i]=c(murders$state[i])
  
}
print(st)

flights %>%
  
  

  
