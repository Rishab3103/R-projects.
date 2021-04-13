library(nycflights13)
?flights
flights
head(flights)
mean(flights$distance)
sum(flights$distance>mean(flights$distance))
sd(flights$distance)
sum(flights$dep_delay>=20, na.rm= TRUE)
f<- flights$dep_delay>=20
sum(f, na.rm = TRUE)

a<-rnorm(n=10, mean = 4, sd=3)
b<-rnorm(n=10, mean = 3, sd=4)
c<-rnorm(n=10, mean = 6, sd=2)
d<-rnorm(n=10, mean = 4, sd=3)
df<- data.frame(a,b,c,d)
df
length(df)
x <- vector(mode="numeric", length = ncol(df))

for(j in 1:ncol(df)){
      x[j]<- median(df[,j])
}

x
df
median(df$a)
lapply(df, median)
city<-c()
cities <- list("Barbados", "Sankt Augustin", "Aachen", "Cologne")
city<-lapply(cities, nchar)
city
getwd()
dir()
web<- read.csv("web-browsers.csv")
web
class(web)
sum(web$hispanic)
broad_perc= (sum(web$broadband)/length((web$broadband)))*100
any_children2<-c()
for(i in 1:length(web$anychildren)){
  if(web$anychildren==1){
    any_children1[i]<-web$spend[i]
  }
  if(web$anychildren==0)
  {
    any_children2[i]<-web$spend[i]
  }
  
}
any_children1
any_children2
mean(any_children1)
mean(any_children2)
sd(web$spend)
max(web$spend)
min(web$spend)
mean(web$spend)
median(web$spend)
quantile(web$spend)
hist(log(web$spend))
plot(log(spend)~ factor(anychildren), data = web)
lm(log(spend)~hispanic, data=web)
plot(lm(log(spend)~ hispanic,data = web))

install.packages("dslabs")
library(dslabs)
?murders
murders
class(murders$state)
murders$region
mean(murders$population)
sum(murders$population>3*mean(murders$population))
state<-c()
c=0
for (i in 1:length(murders$population)) {
  if(murders$population[i]>3*mean(murders$population)){
    c=c+1
    state[c]=c(murders$state[i])
  }
  
}
state
murders$state[1:5]
c=0
state_name<-c()
for(i in 1:length(murders$population)){
  if(murders$population[i]>median(murders$population))
  {
    c=c+1
    state_name[c]= murders$state[i]
  }
}
state_name
murders
table(murders$state,murders$region)
murders[
  with(murders, order(murders$total, murders$state)),
]
murders$state[which.min(murders$total)]
murder_rate<-c()
murder_rate<-(murders$total/murders$population)*100000
murder_rate
murders$murder_rate<-murder_rate
murders
ncol(murders)
state_murder<-c()
c=0
for(i in 1:length(murder_rate)){
  if(murder_rate[i]<0.42)
  {
    c=c+1
    state_murder[c]=murders$state[i]
  }
}
state_murder

