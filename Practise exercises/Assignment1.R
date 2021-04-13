installed.packages("tidyverse")
library(tidyverse)
temps <- c(72, 71,68,73,69,76)
length(temps)
sum(temps)
mean(temps)
sd(temps)
temps[2]
x=1
for (x in 1:6) {
  if(temps[x]<mean(temps))
    print(temps[x])
}
a=temps[2]
print(a)
class(a)
fahr<-c()
for (i in 1:6) {
  fahr[i]= (temps[i]-32)*(5/9)
  
  
}
print(fahr)
u<-rnorm(n=25,mean=0, sd=1)
print(u)
my_ints<-c()
my_ints= round(u,0)
print(my_ints)
my_intsarenotzero<-c()
for (i in 1:25) {
  if(my_ints[i]!=0)
    my_intsarenotzero[i]=my_ints[i]
  
  
}  
print(my_intsarenotzero)

m<-seq(15,77,1)
print(m)

food<-c('pizza', 'burgers', 'salads', 'cheese', 'pasta')
cost<-c(3,2,5,3,4)
names(cost)<- food
print(cost)
print(cost[3:5])
print(mean(cost[1:3]))
sp <- c(23,27,23,21,34)
days<- c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
y=0
mean(sp)
for(i in 1:6){
  if(sp[i]>23)
  
    y=y+1
  
}
print(y)
max(sp)
multiple<-seq(10,40,6)
print(multiple)
pos<-seq(1,100,2)
print(pos)
a=1 
b=1
c=-1
sqrt1= -b+(sqrt((b*b-4*a*c)))/(2*a)
sqrt2= -b-(sqrt((b*b-4*a*c)))/(2*a)
print(sqrt2)
print(sqrt1)
k<-c()
for(i in 1:10)
{
  k[i]=i
  
}
print(k)
city<-c('New York', 'Paris', 'Aachen', 'Cologne')
for(i in 1:4)
{
  if(nchar(city[i])!=6)
    print(city[i])
}
x <- as.integer(runif(1, -10, 10))
print(x)
name1=""
fun_name <- function(name1){
  j="hello there"
  paste(j,name1, sep = "")
  
}
fun_name("Rishab")
product <- function(a,b){
  prod=a*b
  return(prod)
}

product(5,6)

x <- c()
y <- c()
both_na<- function(x,y){
c=0
for(i in 1:5){
  if(is.na(x[i]) && is.na(y[i]))
    c=c+1
}
  return(c)
}

both_na(c( 1, 2, NA, 3, NA),c(NA, 3, NA, 3, 4))
n=0
c=0
prime<-function(n){
  for(i in 1:n){
    if(n%%i==0)
      
      c=c+1
  }
  if(c==2){
    print("Prime Number")
  }
  else{
    print("Composite Number")
  }
}
prime(5)
   
  
