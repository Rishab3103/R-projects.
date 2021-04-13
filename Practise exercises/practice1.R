installed.packages("tidyverse")
library(tidyverse)
temps <- c(72, 71,68,73,69,76)
length(temps)
sum(temps)
mean(temps)
sd(temps)
temps[2]
temps[temps<mean(temps)]
temps[2]<- "a"
class(temps)
temps <- c(72, 71,68,73,69,76)
round((temps-32)*(5/9),0)
?rnorm

my_nums<-rnorm(25,0,1)
my_ints<-as.integer(my_nums)
sum((my_ints!=0))
m<-15:77
length(m)
food <- c("pizza", "burgers", "salads", "cheese", "pasta")
costs<-c(75,80,23,34,56)
food
costs
names(costs)<- food
food
costs
costs[3:5]
mean(costs[1:3])
sp <- c(23,27,23,21,34)
days <- c("Mon","Tues","Wed", "Thu", "Fri")
names(sp)<-days
mean(sp)
sum(sp>23)
max(sp)
sp
seq(from=10, to=40 ,by= 6)
seq(1,100,2)
a <- 1
b <- 1
c <- -1
x1<- (-b + sqrt(b^2 - 4*a*c))/(2*a)
x2<- (-b - sqrt(b^2 - 4*a*c))/(2*a)
for(i in 1:10){
  print(i)
}

cities <- c("New York", "Paris","Aachen", "Cologne")

for(city in cities){
  if(nchar(city)!=6)
  {
    print(city)
  }
  
}
  
x <- as.integer(runif(1, -10, 10))
x
if(x %% 2 ==0){
  print("Even Number")
}else{
  print("Not even")
}

hello_there<-function(name){
  print(paste("Hello", name))
}

hello_there("Rishab")

prod<- function(a,b){
  product= a*b
  print(product)
}

prod(5,6)
x <- c( 1, 2, NA, 3, NA)
y <- c(NA, 3, NA, 3,  4)
is.na(x)
is.na(y)  
sum(is.na(x) & is.na(y))
both_na<-function(a,b){
  b= sum(is.na(a) & is.na(b))
}
x <- c( 1, 2, NA, 3, NA)
y <- c(NA, 3, NA, 3,  4)
print(both_na(x,y))
prime<- function(a){
  c=0
  for (i in 1:a) {
    if(a%%i==0)
      c=c+1
  } 
    if(c>2)
      return(FALSE)
    else
      return(TRUE)
      

}
prime(5)
sum=0
for(i in 1:100){
  if(prime(i)){
    sum=sum+i
  }
}
print(sum)

my_num <- 1:1000
sum(as.numeric(lapply(my_num, prime)))

